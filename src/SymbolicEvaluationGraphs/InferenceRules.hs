{-# LANGUAGE FlexibleContexts #-}

module SymbolicEvaluationGraphs.InferenceRules where

import Data.Maybe
import Data.List
import qualified Data.Map
       (Map, elems, filter, filterWithKey, fromList, union, keys, insert, member, lookup)
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
suc (state@(([],_,Nothing):_),kb) = (tail state, kb)
suc _ = error "Cannot apply 'suc': Malformed AbstractState"

caseRule :: AbstractState -> AbstractState
caseRule ((t:qs,sub,Nothing):s,kb) =
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
    (h', b') <- instantiateWithFreshVariables h b
    let (Just mgu) = unify t h'
    let mguG = restrictSubstToG mgu g
        mguGAndRenaming = restrictSubstToGForU mgu g
    return
        ( ( ( map (apply mgu) (maybe [] splitClauseBody b' ++ qs)
            , compose mgu sub
            , Nothing) :
            map
                (\(t',s',c') ->
                      (map (apply mguG) t', compose mguG s', c'))
                s
          , ((g \\ map Var (Data.Map.keys (toMap mgu))) `union` map
                  Var
                  (concatMap
                       Data.Rewriting.Term.vars
                       (Data.Map.elems (toMap mguG)))
            , map (apply mguGAndRenaming *** apply mguGAndRenaming) u))
        , (s, (g, u ++ [(t, h)])))
eval _ = error "Cannot apply 'eval': Malformed AbstractState"

backtrack :: AbstractState -> AbstractState
backtrack (state@((_,_,Just _):_),kb) = (tail state, kb)

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
    -> Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int (Control.Monad.State.StateT (Data.Map.Map (String,Int,[Int]) [Int]) IO)) (AbstractState, AbstractState)
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
        Control.Monad.State.lift (Control.Monad.State.lift
            (nextG t g >>=
             (\x ->
                   return (g `union` map (apply d) x))))
    return
        ( ([([t], fromMap (Data.Map.fromList []), Nothing)], (g, u))
        , ([(map (apply d) qs, d, Nothing)], (g', map (apply d *** apply d) u)))

nextG :: Term' -> G -> (Control.Monad.State.StateT (Data.Map.Map (String,Int,[Int]) [Int]) IO) G
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
    :: String -> Int -> [Int] -> Control.Monad.State.StateT (Data.Map.Map (String,Int,[Int]) [Int]) IO [Int]
groundnessAnalysis f arity groundInputs = do
  cachedResults <- Control.Monad.State.get
  if (f,arity,groundInputs) `Data.Map.member` cachedResults then
    return (fromJust (Data.Map.lookup (f,arity,groundInputs) cachedResults))
  else do
    groundOutputs <- Control.Monad.State.lift (do
      putStrLn "groundness analysis required:"
      putStrLn ("function symbol: '" ++ f ++ "'" ++ "   arity: " ++ show arity ++ "   ground argument positions: " ++ show groundInputs)
      putStrLn "argument positions that will become ground for every answer substitution?"
      readLn :: IO [Int])
    Control.Monad.State.modify (Data.Map.insert (f,arity,groundInputs) groundOutputs)
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
parallel (ss,kb) = (([head ss], kb), (tail ss, kb))
