module SymbolicEvaluationGraphs.InferenceRules where

import ExprToTerm.Conversion
import Data.Rewriting.Term.Type(Term(..))

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

root :: Term' -> String
root (Var s) = s
root (Fun s _) = s

slice :: [Clause] -> Term' -> [Clause]
slice clauses t = filter (\x -> root (fst x) == root t) clauses

termToClause :: Term' -> Clause
termToClause (Fun ":-" args) = (head args, tail args) -- rule
termToClause h@(Fun _ _) = (h, []) -- fact (empty body)
termToClause _ = error "Cannot apply 'exprToClause': Malformed Clause"
