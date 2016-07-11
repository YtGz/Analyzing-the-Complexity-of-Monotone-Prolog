{-# LANGUAGE FlexibleContexts #-}
module SymbolicEvaluationGraphs.InferenceRules where

import Data.Maybe
import qualified Data.Map (elems, filter, filterWithKey)
import Control.Arrow
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Implicit
import Data.Rewriting.Substitution
import Data.Rewriting.Substitution.Type (fromMap, toMap)
import qualified Data.Rewriting.Term
import Data.Rewriting.Term.Type (Term(..))
import ExprToTerm.Conversion
import SymbolicEvaluationGraphs.Types

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
         , ( map
                 (\(Var v) ->
                       AVar v)
                 (Data.Map.elems
                      (Data.Map.filter
                           (\x ->
                                 case x of
                                     Var _ -> True
                                     _ -> False)
                           (toMap mguG)))
           , map (apply mguG *** apply mguG) u))
       , (s, (g, u ++ [(t, h)]))]
eval _ = error "Cannot apply 'eval': Malformed AbstractState"

root :: Term' -> String
root (Var s) = s
root (Fun s _) = s

slice :: Implicit_ [Clause] => Term' -> [Clause]
slice t =
    filter
        (\x ->
              root (fst x) == root t)
        param_

-- unify, introducing fresh abstract variables
unify'
    :: Term' -> Term' -> Maybe Subst'
unify' t1 t2 =
    let vs = map Var (Data.Rewriting.Term.vars t1)
    in unify
           (Fun "freshVariables" (vs ++ [t2]))
           (Fun "freshVariables" (map freshVariable vs ++ [t1])) -- note the argument order: use unify h t instead of unify t h to ensure mapping from variables (element V) to abstract variables (element A)

counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)

freshVariable :: Term' -> Term' -- TODO: eliminate impurity introduced by using unsafePerformIO
freshVariable _ = Var ("T" ++ show (unsafePerformIO (atomicModifyIORef counter (\x -> (x+1,x)))))

restrictSubstToG :: Subst' -> G -> Subst'
restrictSubstToG sub g =
    fromMap
        (Data.Map.filterWithKey
             (\k _ ->
                   elem (AVar k) g)
             (toMap sub))

-- isSubstCompatibleToKB      G: AVars in G need to map to ground terms (or another AVar from G),   + U???
