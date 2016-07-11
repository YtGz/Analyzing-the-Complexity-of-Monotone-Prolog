module SymbolicEvaluationGraphs.Utilities
  (freshVariable
  ,termToClause
  ,getInitialAbstractState)
  where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import ExprToTerm.Conversion
import Data.Rewriting.Term.Type (Term(..))
import SymbolicEvaluationGraphs.Types
import Query.Utilities
import Data.Rewriting.Substitution.Type (fromMap)
import Data.Map (fromList)

counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)

freshVariable :: Term' -> Term' -- TODO: eliminate impurity introduced by using unsafePerformIO
freshVariable _ =
    Var
        ("T" ++
         show
             (unsafePerformIO
                  (atomicModifyIORef
                       counter
                       (\x ->
                             (x + 1, x)))))

termToClause :: Term' -> Clause
termToClause (Fun ":-" args) = (head args, tail args) -- rule
termToClause h@(Fun _ _) = (h, []) -- fact (empty body)
termToClause _ = error "Cannot apply 'exprToClause': Malformed Clause"

getInitialAbstractState :: QueryClass -> AbstractState
getInitialAbstractState (f,m) =
    ([(Term (Fun f vs), fromMap (fromList []), Nothing)], (gs, []))
  where
    (vs,gs) =
        foldr
            (\x (vs,gs) ->
                  let v = freshVariable (Var "")
                  in case x of
                         In -> (v : vs, v : gs)
                         Out -> (v : vs, gs))
            ([], [])
            m
