{-# OPTIONS_GHC -Wall    #-}
module SymbolicEvaluationGraphs.InferenceRules where

import Control.Arrow
import Control.Monad.Supply
import qualified Control.Monad.State
import Control.Monad.Identity
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List
import qualified Data.Map
       (Map, elems, filter, filterWithKey, fromList, union, keys, insert,
        member, lookup, map)
import Data.Maybe

import Data.Rewriting.Substitution (apply, compose, unify)
import Data.Rewriting.Substitution.Type (fromMap, toMap)
import qualified Data.Rewriting.Term
import Data.Rewriting.Term.Type (Term(..))

import ExprToTerm.Conversion
import SymbolicEvaluationGraphs.Types
import SymbolicEvaluationGraphs.Utilities

suc :: AbstractState -> AbstractState
suc (state@(([],_,Nothing):_),kb) = implicitGeneralization (tail state, kb)
suc _ = error "Cannot apply 'suc': Malformed AbstractState"

caseRule :: [Clause] -> AbstractState -> AbstractState
caseRule clauses ((t:qs,sub,Nothing):s,kb) =
    implicitGeneralization
        ( (let f clauseArray term substitution =
                   if null clauseArray
                       then []
                       else (term, substitution, head clauseArray) :
                            f (tail clauseArray) term substitution
           in f)
              (map Just (slice clauses t))
              (t : qs)
              sub ++
          s
        , kb)
caseRule _ _ = error "Cannot apply 'caseRule': Malformed AbstractState"

eval
    :: Monad m
    => AbstractState
    -> Control.Monad.State.StateT (Int,Int) (Control.Monad.Supply.SupplyT Int (Control.Monad.State.StateT (Data.Map.Map (String, Int, [Int]) [Int]) (Control.Monad.State.StateT (Data.Map.Map Int Subst') m))) (AbstractState, AbstractState)
eval ((t:qs,sub,Just (h,b)):s,(g,u)) = do
    (h_:b'_,_) <- instantiateWithFreshAbstractVariables (h : maybeToList b) --instantiateWithFreshVariables (h : maybeToList b)
    b_ <- renameVariables b'_
    ([t_],freshVarSub) <- instantiateWithFreshAbstractVariables [t]
    let g_ = map (apply freshVarSub) g
        qs_ = map (apply freshVarSub) qs
        sub_ = compose freshVarSub sub
        s_ =
            map
                (\(t',s',c') ->
                      (map (apply freshVarSub) t', compose freshVarSub s', c'))
                s
        u_ = map (apply freshVarSub *** apply freshVarSub) u
        (Just mgu) = unify t_ h_
        mguG = restrictSubstToG mgu g_
        mguGAndRenaming = restrictSubstToGForU mgu g_
    Control.Monad.State.lift
        (Control.Monad.State.lift
             (Control.Monad.State.lift
                  (Control.Monad.State.modify
                       (Data.Map.map
                            (\p ->
                                  fromMap
                                      (toMap p `Data.Map.union`
                                       toMap (applyToSubKeys mgu p)))))))
    let s1 =
            ( ( map
                    (apply mgu)
                    (maybe [] splitClauseBody (listToMaybe b_) ++ qs_)
              , compose mgu sub_
              , Nothing) :
              map
                  (\(t',s',c') ->
                        (map (apply mguG) t', compose mguG s', c'))
                  s_
            , ( (g_ \\ map Var (Data.Map.keys (toMap mgu))) `union`
                map
                    Var
                    (concatMap
                         Data.Rewriting.Term.vars
                         (Data.Map.elems (toMap mguG)))
              , map (apply mguGAndRenaming *** apply mguGAndRenaming) u_))
    return
        ( implicitGeneralization s1
        , implicitGeneralization (s, (g, u ++ [(t, h)])))
eval _ = error "Cannot apply 'eval': Malformed AbstractState"

backtrack :: AbstractState -> AbstractState
backtrack (state@((_,_,Just _):_),kb) = implicitGeneralization (tail state, kb)
backtrack _ =
    error "Backtracking should not be applicable to this abstract state"

root :: Term' -> String
root (Var s) = s
root (Fun s _) = s

slice :: [Clause] -> Term' -> [Clause]
slice clauses t =
    filter
        (\x ->
              root (fst x) == root t)
        clauses

restrictSubstToG :: Subst' -> G -> Subst'
restrictSubstToG sub g =
    fromMap
        (Data.Map.filterWithKey
             (\k _ ->
                   elem (Var k) g)
             (toMap sub))

--is this correct? how else does the instance rule work if we apply sub|G and not sub to U?
restrictSubstToGForU
    :: Subst' -> G -> Subst'
restrictSubstToGForU sub g =
    fromMap
        (Data.Map.union
             (toMap (restrictSubstToG sub g))
             (Data.Map.filter
                  (\v ->
                        Data.Rewriting.Term.isGround v -- ground instance
                         ||
                        Data.Rewriting.Term.isVar v -- pure renaming
                   )
                  (toMap sub)))

implicitGeneralization :: AbstractState -> AbstractState
implicitGeneralization s =
    removeUnnecessaryEntriesFromU (removeUnnecessaryEntriesFromG s)

-- remove term pairs from U for which the left hand side is not equal to (a subterm of) any term of the abstract state
-- TODO: look at example 6.3 from Stroeder: from KB1 to KB2 - how?
removeUnnecessaryEntriesFromU
    :: AbstractState -> AbstractState
removeUnnecessaryEntriesFromU (ss,(g,u)) =
    ( ss
    , ( g
      , filter
            (\(x,_) ->
                  any
                      (\t ->
                            x `elem` Data.Rewriting.Term.subterms t)
                      ts)
            u))
  where
    ts =
        concatMap
            (\(x,_,_) ->
                  x)
            ss

-- remove vars from G which are no longer part of any term of the abstract state
removeUnnecessaryEntriesFromG
    :: AbstractState -> AbstractState
removeUnnecessaryEntriesFromG (ss,(g,u)) = (ss, (filter (`elem` vs) g, u))
  where
    vs =
        map
            Var
            (concatMap
                 Data.Rewriting.Term.vars
                 (concatMap
                      (\(x,_,_) ->
                            x)
                      ss))

-- we can use the backtrack rule if there is no concretization γ w.r.t. KB such that tγ ~ h
isBacktrackingApplicable
    :: AbstractState -> Bool
isBacktrackingApplicable ((t:_,_,Just (h,_)):_,(_,u)) =
    isNothing c ||
    any
        (\(t1,t2) ->
              apply (fromJust c) t1 == apply (fromJust c) t2)
        u
  where
    c = unify t h
isBacktrackingApplicable _ = False

--note that abstract variables are (by definition of the eval rule) necessarily always positioned *directly* below the root (i.e. they appear as arguments of the root function)
arityOfRootSymbol
    :: Term' -> Int
arityOfRootSymbol (Fun _ xs) = length xs
arityOfRootSymbol (Var _) = error "Variables have no arity"

-- split rule for states with a single goal
split
    :: AbstractState
    -> Control.Monad.State.StateT (Int,Int) (Control.Monad.Supply.SupplyT Int (Control.Monad.State.StateT (Data.Map.Map (String, Int, [Int]) [Int]) (Control.Monad.State.StateT (Data.Map.Map Int Subst') IO))) (AbstractState, AbstractState)
split ([(t:qs,_,Nothing)],(g,u)) = do
    let vs =
            map
                Var
                (nub
                     (Data.Rewriting.Term.vars t ++
                      concatMap Data.Rewriting.Term.vars qs)) \\
            g
    freshVariables <- mapM (const freshAbstractVariable) vs
    let d = fromJust (unify (Fun "" vs) (Fun "" freshVariables))
    g' <-
        Control.Monad.State.lift
            (Control.Monad.State.lift
                 (nextG t g >>=
                  (\x ->
                        return (g `union` map (apply d) x))))
    return
        ( implicitGeneralization
              ([([t], fromMap (Data.Map.fromList []), Nothing)], (g, u))
        , implicitGeneralization
              ( [(map (apply d) qs, d, Nothing)]
              , (g', map (apply d *** apply d) u)))
split _ =
    error
        "Error in heuristic. Split rule should not be applicable to this abstract state"

nextG
    :: Term'
    -> G
    -> (Control.Monad.State.StateT (Data.Map.Map (String, Int, [Int]) [Int]) (Control.Monad.State.StateT (Data.Map.Map Int Subst') IO)) G
nextG t g = do
    let gPos =
            filter
                (\p ->
                      null
                          (map
                               Var
                               (nub
                                    (Data.Rewriting.Term.vars
                                         (fromJust
                                              (Data.Rewriting.Term.subtermAt
                                                   t
                                                   [p])))) \\
                           g))
                [0 .. arityOfRootSymbol t - 1]
    gA <- groundnessAnalysis (root t) (arityOfRootSymbol t) gPos
    return
        (nub
             (concatMap
                  (\p ->
                        map
                            Var
                            (Data.Rewriting.Term.vars
                                 (fromJust
                                      (Data.Rewriting.Term.subtermAt t [p]))))
                  gA))

-- groundness analysis (dependent on user input)
groundnessAnalysis
    :: String
    -> Int
    -> [Int]
    -> Control.Monad.State.StateT (Data.Map.Map (String, Int, [Int]) [Int]) (Control.Monad.State.StateT (Data.Map.Map Int Subst') IO) [Int]
groundnessAnalysis f arity groundInputs = do
    cachedResults <- Control.Monad.State.get
    if (f, arity, groundInputs) `Data.Map.member` cachedResults
        then return
                 (fromJust
                      (Data.Map.lookup (f, arity, groundInputs) cachedResults))
        else do
            groundOutputs <-
                Control.Monad.State.liftIO
                    (do putStrLn "groundness analysis required:"
                        putStrLn
                            ("function symbol: '" ++
                             f ++
                             "'" ++
                             "   arity: " ++
                             show arity ++
                             "   ground argument positions: " ++
                             show groundInputs)
                        putStrLn
                            "argument positions that will become ground for every answer substitution?"
                        res <- readLn :: IO [Int]
                        putStrLn ""
                        return res)
            Control.Monad.State.modify
                (Data.Map.insert (f, arity, groundInputs) groundOutputs)
            return groundOutputs

-- contrary to the usual substitution labels, the instance child is annotated by the substitution mu associated to the instance father
tryToApplyInstanceRule
    :: AbstractState
    -> [(AbstractState, (String, Int))]
    -> Maybe (AbstractState, (String, Int))
tryToApplyInstanceRule _ [] = Nothing
tryToApplyInstanceRule n@([(qs,_,c)],(g,u)) ((([(qs',_,c')],(g',u')),(_,i)):xs) =
    if c == c' && length qs == length qs'
        then let mu =
                     nubBy
                         ((==) `on` fmap toMap)
                         (zipWith
                              (\x y ->
                                    maybe
                                        Nothing
                                        (\m ->
                                              if apply m x == y
                                                  then Just
                                                           (fromMap
                                                                (Data.Map.filterWithKey
                                                                     (\var _ ->
                                                                           var `elem`
                                                                           Data.Rewriting.Term.vars
                                                                               x)
                                                                     (toMap m)))
                                                  else Nothing)
                                        (unify x y))
                              qs'
                              qs)
             in if length mu == 1 &&
                   isJust (head mu) &&
                   (\ys zs ->
                         null (ys \\ zs) && null (zs \\ ys))
                       (nub g)
                       (nub
                            (concatMap
                                 (map Var .
                                  Data.Rewriting.Term.vars .
                                  apply (fromJust (head mu)))
                                 g')) &&
                   null
                       (map (join bimap (apply (fromJust (head mu)))) (nub u') \\
                        nub u)
                    then Just
                             ( ([(qs', fromJust (head mu), c')], (g', u'))
                             , ("instanceChild", i))
                    else tryToApplyInstanceRule n xs
        else tryToApplyInstanceRule n xs
tryToApplyInstanceRule n (_:xs) = tryToApplyInstanceRule n xs

parallel :: AbstractState -> (AbstractState, AbstractState)
parallel (ss,kb) =
    ( implicitGeneralization ([head ss], kb)
    , implicitGeneralization (tail ss, kb))
