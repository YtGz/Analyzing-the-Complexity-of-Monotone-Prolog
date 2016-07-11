module SymbolicEvaluationGraphs.Types where

import ExprToTerm.Conversion
import Data.Rewriting.Term.Type (Term(..))

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

termToClause :: Term' -> Clause
termToClause (Fun ":-" args) = (head args, tail args) -- rule
termToClause h@(Fun _ _) = (h, []) -- fact (empty body)
termToClause _ = error "Cannot apply 'exprToClause': Malformed Clause"
