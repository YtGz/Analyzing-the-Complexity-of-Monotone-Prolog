{-# OPTIONS_GHC -Wall    #-}

module SymbolicEvaluationGraphs.Utilities
  (applyToSubKeys
  ,freshVariable
  ,freshAbstractVariable
  ,hasNoAbstractVariables
  ,instantiateWithFreshAbstractVariables
  ,renameVariables
  ,isAbstractVariable
  ,splitClauseBody
  ,termToClause)
  where

import qualified Control.Monad.State
import Data.List (nub)
import Data.Map (Map, fromList, toList, unions, empty)
import Data.Maybe
import Text.Read (readMaybe)

import Data.Rewriting.Substitution (unify, apply)
import Data.Rewriting.Substitution.Type (fromMap, toMap)
import Data.Rewriting.Term (vars)
import Data.Rewriting.Term.Type (Term(..))

import ExprToTerm.Conversion
import SymbolicEvaluationGraphs.Types

freshVariable
    :: Monad m
    => (Control.Monad.State.StateT (Int, Int) m) Term'
freshVariable =
    Control.Monad.State.state
        (\(i,j) ->
              (Var ("X" ++ show j), (i, j + 1)))

freshAbstractVariable
    :: Monad m
    => (Control.Monad.State.StateT (Int, Int) m) Term'
freshAbstractVariable =
    Control.Monad.State.state
        (\(i,j) ->
              (Var ("T" ++ show i), (i + 1, j)))

-- Replace all variables of the head of a clause with fresh abstract variables
instantiateWithFreshAbstractVariables
    :: Monad m
    => [Term'] -> Control.Monad.State.StateT (Int, Int) m ([Term'], Subst')
instantiateWithFreshAbstractVariables ts =
    let vs = map Var (nub (Data.Rewriting.Term.vars (head ts)))
    in do freshVariables <- mapM (const freshAbstractVariable) vs
          let sub = fromJust (unify (Fun "" vs) (Fun "" freshVariables))
          return (map (apply sub) ts, sub)

-- Replace all non-abstract variables with fresh (non-abstract) variables
renameVariables
    :: Monad m
    => [Term'] -> Control.Monad.State.StateT (Int, Int) m [Term']
renameVariables ts = do
    let vs = nub (concatMap vars ts)
    sub <-
        Control.Monad.State.liftM
            fromMap
            (Control.Monad.State.liftM
                 fromList
                 (Control.Monad.State.liftM2
                      zip
                      (return vs)
                      (mapM
                           (\v ->
                                 if isAbstractVariable v
                                     then return (Var v)
                                     else freshVariable)
                           vs)))
    return (map (apply sub) ts)

-- abstract variables have the format "T" ++ [Int]
isAbstractVariable
    :: String -> Bool
isAbstractVariable (x:xs) = x == 'T' && isJust (readMaybe xs :: Maybe Int)
isAbstractVariable _ = error "Malformed term"

-- check if no variable in the input program is of the format of our abstract variables
hasNoAbstractVariables
    :: Term' -> Bool
hasNoAbstractVariables t = not (any isAbstractVariable (vars t))

termToClause :: Term' -> Clause
termToClause (Fun ":-" args) = (head args, Just (head (tail args))) -- rule
termToClause h@(Fun _ _) = (h, Nothing) -- fact (empty body)
termToClause _ = error "Cannot apply 'exprToClause': Malformed Clause"

splitClauseBody :: Term' -> [Term']
splitClauseBody (Fun "," bs) = concatMap splitClauseBody bs
splitClauseBody b = [b]

applyToSubKeys :: Subst' -> Subst' -> Subst'
applyToSubKeys s s' = fromMap (applyToSubKeys_ (toList (toMap s)) s')

applyToSubKeys_ :: [(String, Term')] -> Subst' -> Map String Term'
applyToSubKeys_ [] _ = Data.Map.empty
applyToSubKeys_ (s:ss) s' =
    Data.Map.unions
        (applyToSubKeys_ ss s' :
         map
             (applyToSubKeys__ (snd s) . snd)
             (filter
                  (\(x,_) ->
                        x == fst s)
                  (toList (toMap s'))))

applyToSubKeys__ :: Term' -> Term' -> Map String Term'
applyToSubKeys__ (Var x) f = fromList [(x, f)]
applyToSubKeys__ (Fun f args) (Fun f' args') =
    if f == f' && length args == length args'
        then Data.Map.unions (zipWith applyToSubKeys__ args args')
        else Data.Map.empty
applyToSubKeys__ _ _ = Data.Map.empty
