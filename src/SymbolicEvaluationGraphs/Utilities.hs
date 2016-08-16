module SymbolicEvaluationGraphs.Utilities
  (freshVariable
  ,isAbstractVariable
  ,hasNoAbstractVariables
  ,termToClause
  ,splitClauseBody)
  where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Monad.State
import Control.Monad.Identity
import ExprToTerm.Conversion
import Data.Rewriting.Term.Type (Term(..))
import SymbolicEvaluationGraphs.Types
import Data.Rewriting.Substitution.Type (fromMap)
import Data.Rewriting.Term (vars)
import Text.Read (readMaybe)
import Data.Maybe

{-counter :: IORef Int
{-# NOINLINE counter #-} -}

{-counter = unsafePerformIO (newIORef 0)

freshVariable :: Term' -> Term' -- TODO: eliminate impurity introduced by using unsafePerformIO
freshVariable _ =
    Var
        ("T" ++
         show
             (unsafePerformIO
                  (atomicModifyIORef
                       counter
                       (\x ->
                             (x + 1, x)))))-}

freshVariable :: (Monad m) => Control.Monad.State.StateT Int m Term'
freshVariable = Control.Monad.State.state (\i -> (Var ("T" ++ show i),i+1))

-- abstract variables have the format "T" ++ [Int]
isAbstractVariable
    :: String -> Bool
isAbstractVariable (x:xs) = x == 'T' && isJust (readMaybe xs :: Maybe Int)

-- check if no variable in the input program is of the format of our abstract variables
hasNoAbstractVariables
    :: [Clause] -> Bool
hasNoAbstractVariables clauses =
    let f t = not (any isAbstractVariable (vars t))
    in all
           (\(t,ts) ->
                 f t && all f ts)
           clauses

termToClause :: Term' -> Clause
termToClause (Fun ":-" args) = (head args, Just (head (tail args))) -- rule
termToClause h@(Fun _ _) = (h, Nothing) -- fact (empty body)
termToClause _ = error "Cannot apply 'exprToClause': Malformed Clause"

splitClauseBody :: Term' -> [Term']
splitClauseBody (Fun "," bs) = bs
splitClauseBody b = [b]
