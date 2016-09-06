{-# LANGUAGE FlexibleContexts #-}

module SymbolicEvaluationGraphs.InferenceRules where

import Data.Maybe
import Data.List
import qualified Data.Map
       (Map, elems, filter, filterWithKey, fromList, union, keys, insert,
        member, lookup, empty, findWithDefault, map, unions)
import Control.Arrow
import Control.Monad (join)
import Control.Monad.Supply
import qualified Control.Monad.State
import Control.Monad.Identity
import Data.Implicit
import Data.Bifunctor (bimap)
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)
import Data.Rewriting.Substitution (apply, compose, unify)
import Data.Rewriting.Substitution.Type (fromMap, toMap)
import qualified Data.Rewriting.Term
import Data.Rewriting.Term.Type (Term(..))
import ExprToTerm.Conversion
import SymbolicEvaluationGraphs.Types
import SymbolicEvaluationGraphs.Utilities
import Diagrams.TwoD.Layout.Tree

suc :: AbstractState -> AbstractState
suc (state@(([],_,Nothing):_),kb) = implicitGeneralization (tail state, kb)
suc _ = error "Cannot apply 'suc': Malformed AbstractState"

caseRule :: AbstractState -> AbstractState
caseRule ((t:qs,sub,Nothing):s,kb) =
    implicitGeneralization
        ( (let f clauseArray term substitution =
                   if null clauseArray
                       then []
                       else (term, substitution, head clauseArray) :
                            f (tail clauseArray) term substitution
           in f)
              (map Just (slice t))
              (t : qs)
              sub ++
          s
        , kb)
caseRule _ = error "Cannot apply 'caseRule': Malformed AbstractState"

eval
    :: (Monad m)
    => AbstractState
    -> Control.Monad.State.StateT Int m (AbstractState, AbstractState)
eval ((t:qs,sub,Just (h,b)):s,(g,u)) = do
    (h_:b_,_) <- instantiateWithFreshVariables (h : maybeToList b)
    ([t_],freshVarSub) <- instantiateWithFreshVariables [t]
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
    let s1 =
            ( ( map
                    (apply mgu)
                    (maybe [] splitClauseBody (listToMaybe b_) ++ qs)
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

flattenLists
    :: (Monad m)
    => AbstractState -> Control.Monad.State.StateT Int m AbstractState
flattenLists ((ts,s,c):ss,(g,u)) = do
    sub <-
        Control.Monad.State.liftM
            Data.Map.unions
            (liftM2
                 (++)
                 (mapM (`flattenListsInTerm` Data.Map.empty) ts)
                 (mapM
                      (`flattenListsInTerm` Data.Map.empty)
                      (Data.Map.elems (toMap s))))
    return
        ( ( map (`applyFlattening` sub) ts
          , fromMap (Data.Map.map (`applyFlattening` sub) (toMap s))
          , c) :
          ss
        , ( applyFlatteningToG g sub ((ts, s, c) : ss)
          , map
                ((`applyFlattening` Data.Map.filterWithKey
                                        (\k _ ->
                                              case k of
                                                  Fun ":" [_,Fun "[]" []] ->
                                                      False
                                                  _ -> True)
                                        sub) ***
                 (`applyFlattening` Data.Map.filterWithKey
                                        (\k _ ->
                                              case k of
                                                  Fun ":" [_,Fun "[]" []] ->
                                                      False
                                                  _ -> True)
                                        sub))
                u))

flattenListsInTerm
    :: (Monad m)
    => Term'
    -> Data.Map.Map Term' Term'
    -> Control.Monad.State.StateT Int m (Data.Map.Map Term' Term')
--flattenListsInTerm (Fun ":" [_,Fun"[]" []]) = flatten that too ++ flatten generally anything with x:[] => x
flattenListsInTerm (Fun ":" subterms) m =
    Control.Monad.State.liftM
        Data.Map.unions
        (mapM (`flattenListsInTerm_` m) subterms)
flattenListsInTerm (Fun f subterms) m =
    Control.Monad.State.liftM
        Data.Map.unions
        (mapM (`flattenListsInTerm` m) subterms)
flattenListsInTerm _ m = return m

flattenListsInTerm_
    :: (Monad m)
    => Term'
    -> Data.Map.Map Term' Term'
    -> Control.Monad.State.StateT Int m (Data.Map.Map Term' Term')
flattenListsInTerm_ l@(Fun ":" _) m = do
    v <- freshVariable
    return (Data.Map.insert l v m)
flattenListsInTerm_ (Fun f subterms) m =
    Control.Monad.State.liftM
        Data.Map.unions
        (mapM (`flattenListsInTerm_` m) subterms)
flattenListsInTerm_ _ m = return m

applyFlattening :: Term' -> Data.Map.Map Term' Term' -> Term'
applyFlattening v@(Var _) sub = Data.Map.findWithDefault v v sub
applyFlattening f@(Fun _ _) sub =
    let f' = Data.Map.findWithDefault f f sub
    in case f' of
           Var _ -> f'
           Fun f'' args -> Fun f'' (map (`applyFlattening` sub) args)

applyFlatteningToG
    :: [Term']
    -> Data.Map.Map Term' Term'
    -> SymbolicEvaluationGraphs.Types.State
    -> [Term']
applyFlatteningToG g sub ((ts,_,_):ss) =
    filter
        (`elem` map
                    Var
                    (nub
                         (concatMap
                              Data.Rewriting.Term.vars
                              (map (`applyFlattening` sub) ts ++
                               concatMap
                                   (\(x,_,_) ->
                                         x)
                                   ss))))
        g ++
    Data.Map.elems
        (Data.Map.filterWithKey
             (\k _ ->
                   all (`elem` g) (map Var (Data.Rewriting.Term.vars k)))
             sub)

backtrack :: AbstractState -> AbstractState
backtrack (state@((_,_,Just _):_),kb) = implicitGeneralization (tail state, kb)

root :: Term' -> String
root (Var s) = s
root (Fun s _) = s

slice
    :: Implicit_ [Clause]
    => Term' -> [Clause]
slice t =
    filter
        (\x ->
              root (fst x) == root t)
        param_

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
isBacktrackingApplicable ((t:_,_,Just (h,_)):_,(g,u)) =
    isNothing c || isNothing (unify (apply (fromJust c) t) h)
  where
    c' =
        fromMap
            (Data.Map.fromList
                 (map
                      (\(t@(Var v),p) ->
                            let s =
                                    fromJust
                                        (Data.Rewriting.Term.subtermAt h p)
                            in if notElem t g || Data.Rewriting.Term.isGround s
                                   then (v, s)
                                   else ( v
                                        , fromMap
                                              (Data.Map.fromList
                                                   (map
                                                        (\x ->
                                                              (x, Fun "" []))
                                                        (Data.Rewriting.Term.vars
                                                             s))) `apply`
                                          s))
                      (filter
                           (\x ->
                                 case x of
                                     (Var _,_) -> True
                                     _ -> False)
                           (map
                                (\p ->
                                      ( fromJust
                                            (Data.Rewriting.Term.subtermAt
                                                 t
                                                 [p])
                                      , [p]))
                                [0 .. arityOfRootSymbol t - 1]))))
    c   -- check for compatibility with U
     =
        if any
               (\(t1,t2) ->
                     isJust (unify (apply c' t1) (apply c' t2)))
               u
            then Nothing
            else Just c'

--note that abstract variables are (by definition of the eval rule) necessarily always positioned *directly* below the root (i.e. they appear as arguments of the root function)
arityOfRootSymbol
    :: Term' -> Int
arityOfRootSymbol (Fun _ xs) = length xs

-- split rule for states with a single goal
split
    :: AbstractState
    -> Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int (Control.Monad.State.StateT (Data.Map.Map (String, Int, [Int]) [Int]) IO)) (AbstractState, AbstractState)
split ([(t:qs,sub,Nothing)],(g,u)) = do
    let vs =
            map
                Var
                (nub
                     (Data.Rewriting.Term.vars t ++
                      concatMap Data.Rewriting.Term.vars qs)) \\
            g
    freshVariables <- mapFreshVariables (return vs)
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

nextG
    :: Term'
    -> G
    -> (Control.Monad.State.StateT (Data.Map.Map (String, Int, [Int]) [Int]) IO) G
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
    -> Control.Monad.State.StateT (Data.Map.Map (String, Int, [Int]) [Int]) IO [Int]
groundnessAnalysis f arity groundInputs = do
    cachedResults <- Control.Monad.State.get
    if (f, arity, groundInputs) `Data.Map.member` cachedResults
        then return
                 (fromJust
                      (Data.Map.lookup (f, arity, groundInputs) cachedResults))
        else do
            groundOutputs <-
                Control.Monad.State.lift
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
tryToApplyInstanceRule n@([(qs,_,c)],(g,u)) ((([(qs',_,c')],(g',u')),(r,i)):xs) =
    if c == c' && length qs == length qs'
        then let mu = nubBy ((==) `on` fmap toMap) (zipWith unify qs' qs)
             in if length mu == 1 &&
                   isJust (head mu) &&
                   (\xs ys ->
                         null (xs \\ ys) && null (ys \\ xs))
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
