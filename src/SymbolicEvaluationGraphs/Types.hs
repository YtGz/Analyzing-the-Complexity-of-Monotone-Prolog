{-# OPTIONS_GHC -Wall #-}
module SymbolicEvaluationGraphs.Types where

import ExprToTerm.Conversion

type AbstractState = (State, KnowledgeBase)

type KnowledgeBase = (G, U)

type G = [AbstractVariable]

type AbstractVariable = Term'

type U = [(Term', Term')]

type State = [([Term'], Subst', Maybe Clause)]

type Clause = (Term', Maybe Term') -- h :- B
