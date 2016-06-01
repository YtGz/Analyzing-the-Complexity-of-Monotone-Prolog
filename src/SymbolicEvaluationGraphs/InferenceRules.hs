module SymbolicEvaluationGraphs.InferenceRules where

import Language.Prolog.Syntax

type AbstractState = (State, KnowledgeBase)
type KnowledgeBase = (G, U)
type G = [AbstractVariable]
data AbstractVariable = AVar String deriving(Show)
type U = [(Term, Term)]
type Term = Expr
type State = [(Goal, Substitution)]
data Goal = Term Term | Hole deriving(Show)
type Substitution = String --TODO: find better type
type Clause = (Term, [Term]) -- h :- B

suc :: AbstractState -> AbstractState
suc (state@((Hole,_):_),kb) = (tail state, kb)
suc _ = error "Cannot apply 'suc': Malformed AbstractState"


root :: Term -> String
root (Var s) = s
root (Str s _) = s
root (Op s _) = s
root (Cons _ _) = ":"
root (Num e) = show e

slice :: [Clause] -> Term -> [Clause]
slice clauses t = filter (\x -> root (fst x) == root t) clauses

exprToClause :: Term -> Clause
exprToClause h@(Str _ _) = (h, []) -- fact (empty body)
exprToClause (Op ":-" args) = (head args, tail args) -- rule
exprToClause _ = error "Cannot apply 'exprToClause': Malformed Clause"
