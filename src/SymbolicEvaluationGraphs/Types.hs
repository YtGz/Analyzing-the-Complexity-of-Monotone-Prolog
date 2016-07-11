module SymbolicEvaluationGraphs.Types where

import ExprToTerm.Conversion
import Data.Rewriting.Term.Type (Term(..))

type AbstractState = (State, KnowledgeBase)

type KnowledgeBase = (G, U)

type G = [AbstractVariable]

type AbstractVariable = Term'

type U = [(Term', Term')]

type State = [(Goal, Subst', Maybe Clause)]

data Goal
    = Term Term'
    | Hole
    deriving (((((Show)))))

type Clause = (Term', [Term']) -- h :- B
