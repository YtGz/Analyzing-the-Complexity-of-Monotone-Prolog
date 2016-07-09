module SymbolicEvaluationGraphs.InferenceRules where

import Data.Maybe
import qualified Data.Map (elems, filter, filterWithKey)
import Control.Arrow
import Data.Rewriting.Substitution
import Data.Rewriting.Substitution.Type (fromMap, toMap)
import qualified Data.Rewriting.Term
import Data.Rewriting.Term.Type (Term(..))
import ExprToTerm.Conversion

type AbstractState = (State, KnowledgeBase)

type KnowledgeBase = (G, U)

type G = [AbstractVariable]

data AbstractVariable =
    AVar String
    deriving (Eq,Show)

type U = [(Term', Term')]

type State = [(Goal, Subst', Maybe Clause)]

data Goal
    = Term Term'
    | Hole
    deriving ((((Show))))

type Clause = (Term', [Term']) -- h :- B

suc :: AbstractState -> AbstractState
suc (state@((Hole,_,Nothing):_),kb) = (tail state, kb)
suc _ = error "Cannot apply 'suc': Malformed AbstractState"

caseRule :: [Clause] -> AbstractState -> AbstractState
caseRule clauses ((Term t,sub,Nothing):s,kb) =
    ( (let f clauseArray term substitution =
               if null clauseArray
                   then []
                   else (Term term, substitution, head clauseArray) :
                        f (tail clauseArray) term substitution
       in f)
          (map Just (slice clauses t))
          t
          sub ++
      s
    , kb)
caseRule _ _ = error "Cannot apply 'caseRule': Malformed AbstractState"

eval :: [Clause] -> AbstractState -> [AbstractState] --TODO: apply mgu also to KB
eval clauses ((Term t,sub,Just (h,b)):s,(g,u)) =
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
eval _ _ = error "Cannot apply 'eval': Malformed AbstractState"

root :: Term' -> String
root (Var s) = s
root (Fun s _) = s

slice :: [Clause] -> Term' -> [Clause]
slice clauses t =
    filter
        (\x ->
              root (fst x) == root t)
        clauses

termToClause :: Term' -> Clause
termToClause (Fun ":-" args) = (head args, tail args) -- rule
termToClause h@(Fun _ _) = (h, []) -- fact (empty body)
termToClause _ = error "Cannot apply 'exprToClause': Malformed Clause"

-- unify, introducing fresh abstract variables
unify'
    :: Term' -> Term' -> Maybe Subst'
unify' t1 t2 =
    let vs = map Var (Data.Rewriting.Term.vars t1)
    in unify
           (Fun "freshVariables" (vs ++ [t2]))
           (Fun "freshVariables" (map freshVariable vs ++ [t1])) -- note the argument order: use unify h t instead of unify t h to ensure mapping from variables (element V) to abstract variables (element A)

freshVariable :: Term' -> Term'
freshVariable (Var v) = Var (v ++ "'") -- TODO: find alternative way of renaming that is more readable (e.g. with static counter)

restrictSubstToG :: Subst' -> G -> Subst'
restrictSubstToG sub g =
    fromMap
        (Data.Map.filterWithKey
             (\k _ ->
                   elem (AVar k) g)
             (toMap sub))

-- isSubstCompatibleToKB      G: AVars in G need to map to ground terms (or another AVar from G),   + U???
