module SymbolicEvaluationGraphs.InferenceRules where

import ExprToTerm.Conversion
import Data.Rewriting.Term.Type(Term(..))
import qualified Data.Rewriting.Term
import Data.Rewriting.Substitution
import Data.Maybe

type AbstractState = (State, KnowledgeBase)
type KnowledgeBase = (G, U)
type G = [AbstractVariable]
data AbstractVariable = AVar String deriving(Show)
type U = [(Term', Term')]
type State = [(Goal, Subst', Maybe Clause)]
data Goal = Term Term' | Hole deriving(Show)
type Clause = (Term', [Term']) -- h :- B

suc :: AbstractState -> AbstractState
suc (state@((Hole,_,Nothing):_),kb) = (tail state, kb)
suc _ = error "Cannot apply 'suc': Malformed AbstractState"

caseRule :: [Clause] -> AbstractState -> AbstractState
caseRule clauses ((Term t,sub,Nothing):s, kb) = ((let f clauseArray term substitution = if null clauseArray then [] else (Term term, substitution, head clauseArray) : f (tail clauseArray) term substitution in f) (map Just (slice clauses t)) t sub ++ s,kb)

eval :: [Clause] -> AbstractState -> AbstractState
eval clauses ((Term t,sub,Just (h,_)):s, (g,u)) = let mgu = unify t h in if isNothing mgu then (s, (g, u++[(t, h)])) else (s,(g,u))

root :: Term' -> String
root (Var s) = s
root (Fun s _) = s

slice :: [Clause] -> Term' -> [Clause]
slice clauses t = filter (\x -> root (fst x) == root t) clauses

termToClause :: Term' -> Clause
termToClause (Fun ":-" args) = (head args, tail args) -- rule
termToClause h@(Fun _ _) = (h, []) -- fact (empty body)
termToClause _ = error "Cannot apply 'exprToClause': Malformed Clause"

-- unify, introducing fresh abstract variables
unify' :: Term' -> Term' -> Maybe Subst'
unify' t1 t2 = let vs = map Var (Data.Rewriting.Term.vars t1) in unify (Fun "freshVariables" (vs ++ [t2])) (Fun "freshVariables" (map freshVariable vs ++ [t1])) -- note the argument order: use unify h t instead of unify t h to ensure mapping from variables (element V) to abstract variables (element A)

freshVariable :: Term' -> Term'
freshVariable (Var v) = Var (v ++ "'") -- TODO: find alternative way of renaming that is more readable (e.g. with static counter)

-- isSubstCompatibleToKB      G: AVars in G need to map to ground terms (or another AVar from G),   + U???
