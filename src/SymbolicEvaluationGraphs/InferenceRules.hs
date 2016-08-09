{-# LANGUAGE FlexibleContexts #-}

module SymbolicEvaluationGraphs.InferenceRules where

import Data.Maybe
import Data.List
import qualified Data.Map (elems, filter, filterWithKey, fromList)
import Control.Arrow
import Data.Implicit
import Data.Function (on)
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

eval :: AbstractState -> (AbstractState, AbstractState)
eval ((t:qs,sub,Just (h,b)):s,(g,u)) =
    let (Just mgu) = unify' t h
        mguG = restrictSubstToG mgu g
    in ( ( ( map (apply mgu) (splitClauseBody (fromJust b) ++ qs)
           , compose sub mgu
           , Nothing) :
           map
               (\(t',s',c') ->
                     (map (apply mguG) t', compose s' mguG, c'))
               s
         , ( map
                 Var
                 (concatMap
                      Data.Rewriting.Term.vars
                      (Data.Map.elems (toMap mguG)))
           , map (apply mguG *** apply mguG) u))
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

-- unify, introducing fresh abstract variables
unify'
    :: Term' -> Term' -> Maybe Subst'
unify' t1 t2
  | Just s <- unify t2 t1   -- note the argument order: use unify h t instead of unify t h to ensure mapping from variables (element V) to abstract variables (element A)
   =
      let vs =
              map
                  Var
                  (nub
                       (concatMap
                            Data.Rewriting.Term.vars
                            (Data.Map.elems (toMap s))))
      in Just
             (fromJust (unify (Fun "" vs) (Fun "" (map freshVariable vs))) `compose`
              s) -- compose takes arguments in reverse order!!!

restrictSubstToG :: Subst' -> G -> Subst'
restrictSubstToG sub g =
    fromMap
        (Data.Map.filterWithKey
             (\k _ ->
                   elem (Var k) g)
             (toMap sub))

applyRules :: AbstractState -> BTree (AbstractState, String)
applyRules s@([],_) = leaf (s, "") -- just for output (base case of recursion)
applyRules s@(([],_,_):_,_) = BNode (s, "suc") (applyRules (suc s)) Empty
applyRules s@((_,_,Nothing):_,_) =
    BNode (s, "case") (applyRules (caseRule s)) Empty
applyRules s =
    if isBacktrackingApplicable s
        then BNode (s, "backtrack") (applyRules (backtrack s)) Empty
        else BNode (s, "eval") (applyRules s1) (applyRules s2)
  where
    ss = eval s
    s1 = fst ss
    s2 = snd ss

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
    :: AbstractState -> (AbstractState, AbstractState)
split ([(t:qs,sub,Nothing)],(g,u)) =
    ( ([([t], fromMap (Data.Map.fromList []), Nothing)], (g, u))
    , ([(map (apply d) qs, d, Nothing)], (g', map (apply d *** apply d) u)))
  where
    vs =
        map
            Var
            (nub
                 (Data.Rewriting.Term.vars t ++
                  concatMap Data.Rewriting.Term.vars qs)) \\
        g
    d = fromJust (unify (Fun "" vs) (Fun "" (map freshVariable vs)))
    g' = g `union` map (apply d) (nextG t g)

nextG :: Term' -> G -> G
nextG t g =
    nub
        (concatMap
             (\p ->
                   map
                       Var
                       (Data.Rewriting.Term.vars
                            (fromJust (Data.Rewriting.Term.subtermAt t [p]))))
             (groundnessAnalysis (root t) gPos))
  where
    gPos =
        filter
            (\p ->
                  map
                      Var
                      (nub
                           (Data.Rewriting.Term.vars
                                (fromJust (Data.Rewriting.Term.subtermAt t [p])))) ==
                  g)
            [0 .. arityOfRootSymbol t - 1]

-- mocking groundness analysis
groundnessAnalysis
    :: String -> [Int] -> [Int]
groundnessAnalysis f groundInputs = groundInputs

tryToApplyInstanceRule :: AbstractState
                       -> [AbstractState]
                       -> Maybe AbstractState
tryToApplyInstanceRule ((qs,_,_):_,(g,u)) =
    find
        (\((qs',_,_):_,(g',u')) ->
              length qs == length qs' &&
              let mu = nubBy ((==) `on` fmap toMap) (zipWith unify qs' qs)
              in length mu == 1 &&
                 isJust (head mu) &&
                 (\xs ys ->
                       null (xs \\ ys) && null (ys \\ xs))
                     (nub g)
                     (nub
                          (concatMap
                               (map Var . Data.Rewriting.Term.vars . apply (fromJust (head mu)))
                               g')) &&
                 null (map (fmap (apply (fromJust (head mu)))) u' \\ u))

parallel :: AbstractState -> (AbstractState, AbstractState)
parallel (ss, kb) = (([head ss], kb), (tail ss, kb)) 
