module SymbolicEvaluationGraphs.Utilities
  (freshVariable
  ,isAbstractVariable
  ,hasNoAbstractVariables
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
import Data.Rewriting.Term (vars)
import Data.Map (fromList)
import Text.Read (readMaybe)
import Data.Maybe

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

-- abstract variables have the format "T" ++ [Int]
isAbstractVariable :: String -> Bool
isAbstractVariable (x:xs) = x == 'T' && isJust (readMaybe xs :: Maybe Int)

-- check if no variable in the input program is of the format of our abstract variables
hasNoAbstractVariables :: [Clause] -> Bool
hasNoAbstractVariables clauses =
    let f t = not (any isAbstractVariable (vars t))
    in all
           (\(t,ts) ->
                 f t && all f ts)
           clauses

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
