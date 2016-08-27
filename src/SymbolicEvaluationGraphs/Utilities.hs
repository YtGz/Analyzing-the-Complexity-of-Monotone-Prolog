module SymbolicEvaluationGraphs.Utilities
  (freshVariable
  ,instantiateWithFreshVariables
  ,mapFreshVariables
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
import Data.Rewriting.Substitution (unify, apply)
import Data.Rewriting.Substitution.Type (fromMap)
import Data.Rewriting.Term (vars)
import Text.Read (readMaybe)
import Data.Maybe
import Data.List (nub)

freshVariable :: (Monad m) => Control.Monad.State.StateT Int m Term'
freshVariable = Control.Monad.State.state (\i -> (Var ("T" ++ show i),i+1))

{-instantiateWithFreshVariables
  :: (Monad m)
  => Term' -> Maybe Term' -> Control.Monad.State.StateT Int m (Term', Maybe Term')
instantiateWithFreshVariables h b =
  let vs =
          map
              Var
              (nub
                        (Data.Rewriting.Term.vars
                        h ++ maybe [] Data.Rewriting.Term.vars b) )
  in do freshVariables <- mapFreshVariables (return vs)
        let sub = fromJust (unify (Fun "" vs) (Fun "" freshVariables))
        return (apply sub h, fmap (apply sub) b)-}

instantiateWithFreshVariables
  :: (Monad m)
  => [Term'] -> Control.Monad.State.StateT Int m ([Term'], Subst')
instantiateWithFreshVariables ts =
  let vs =
          map
              Var
              (nub
                        (concatMap Data.Rewriting.Term.vars
                        ts))
  in do freshVariables <- mapFreshVariables (return vs)
        let sub = fromJust (unify (Fun "" vs) (Fun "" freshVariables))
        return (map (apply sub) ts, sub)

--TODO: there has to be a higher-order function that can be used instead
mapFreshVariables
    :: (Monad m)
    => Control.Monad.State.StateT Int m [Term']
    -> Control.Monad.State.StateT Int m [Term']
mapFreshVariables s = do
    l <- s
    case l of
        [] -> return []
        (_:xs) -> do
            v <- freshVariable
            vs <- mapFreshVariables (return xs)
            return (v : vs)

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
splitClauseBody (Fun "," bs) = concatMap splitClauseBody bs
splitClauseBody b = [b]
