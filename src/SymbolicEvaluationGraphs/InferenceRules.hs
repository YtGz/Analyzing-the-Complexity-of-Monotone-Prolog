{-# LANGUAGE FlexibleContexts #-}

module SymbolicEvaluationGraphs.InferenceRules where

import Data.Maybe
import Data.List
import qualified Data.Map (elems, filter, filterWithKey, fromList)
import Control.Arrow
import Data.Implicit
import Data.Rewriting.Substitution (apply, compose, unify)
import Data.Rewriting.Substitution.Type (fromMap, toMap)
import qualified Data.Rewriting.Term
import Data.Rewriting.Term.Type (Term(..))
import ExprToTerm.Conversion
import SymbolicEvaluationGraphs.Types
import SymbolicEvaluationGraphs.Utilities

suc :: AbstractState -> AbstractState
suc (state@((Hole,_,Nothing):_),kb) = (tail state, kb)
suc _ = error "Cannot apply 'suc': Malformed AbstractState"

caseRule :: AbstractState -> AbstractState
caseRule ((Term t,sub,Nothing):s,kb) =
    ( (let f clauseArray term substitution =
               if null clauseArray
                   then []
                   else (Term term, substitution, head clauseArray) :
                        f (tail clauseArray) term substitution
       in f)
          (map Just (slice t))
          t
          sub ++
      s
    , kb)
caseRule _ = error "Cannot apply 'caseRule': Malformed AbstractState"

eval :: AbstractState -> [AbstractState] --TODO: apply mgu also to KB
eval ((Term t,sub,Just (h,b)):s,(g,u)) =
    let (Just mgu) = unify' t h
        mguG = restrictSubstToG mgu g
    in [ ( map
               (\x ->
                     (Term (apply mgu x), compose sub mgu, Nothing))
               b ++
           map
               (\(Term t',s',c') ->
                     (Term (apply mguG t'), compose s' mguG, c'))
               s  {-use new abstractVars-}
         , ( Data.Map.elems
                 (Data.Map.filter
                      (\x ->
                            case x of
                                Var _ -> True
                                _ -> False)
                      (toMap mguG))
           , map (apply mguG *** apply mguG) u))
       , (s, (g, u ++ [(t, h)]))]
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

applyRule :: AbstractState -> [AbstractState]
applyRule s@([],_) = [s]
applyRule s@((Hole,_,_):_,_) = applyRule (suc s)
applyRule s@((_,_,Nothing):_,_) = applyRule (caseRule s)
applyRule s =
    if isBacktrackingApplicable s
        then applyRule (backtrack s)
        else applyRule s1 ++ applyRule s2
  where
    ss = eval s
    s1 = head ss
    s2 = head (tail ss)

-- we can use the backtrack rule if there is no concretization γ w.r.t. KB such that tγ ~ h
isBacktrackingApplicable
    :: AbstractState -> Bool
isBacktrackingApplicable ((Term t,_,Just (h,_)):_,(g,u)) =
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
                                [0 .. arityOfRootSymbol t]))))
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
