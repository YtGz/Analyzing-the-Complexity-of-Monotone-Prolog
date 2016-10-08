{-# OPTIONS_GHC -Wall   #-}
module Query.Utilities
  (ArgumentType(In, Out)
  ,QueryClass
  ,getQueryClasses
  ,isQuery)
  where

import qualified Data.List

import qualified Language.Prolog.Syntax

isQuery :: Language.Prolog.Syntax.Expr -> Bool
isQuery (Language.Prolog.Syntax.Op ":-" [_]) = True
isQuery _ = False

------------------------------------------------------------------------------
-- query classes
data ArgumentType
    = In
    | Out
    deriving (Show,Eq)

type FunctorName = String

type QueryClass = (FunctorName, [ArgumentType])

getQueryClasses :: [Language.Prolog.Syntax.Expr] -> [QueryClass]
getQueryClasses exprs =
    Data.List.nub (map getQueryClass (filter isQuery exprs))

getQueryClass :: Language.Prolog.Syntax.Expr -> QueryClass
getQueryClass (Language.Prolog.Syntax.Op _ [Language.Prolog.Syntax.Str functor args]) =
    (functor, map getArgumentType args)
getQueryClass _ = error "Malformed query"

getArgumentType :: Language.Prolog.Syntax.Expr -> ArgumentType
getArgumentType (Language.Prolog.Syntax.Str _ []) = In
getArgumentType (Language.Prolog.Syntax.Str _ args) =
    if Out `elem` map getArgumentType args
        then Out
        else In
getArgumentType (Language.Prolog.Syntax.Num _) = In
getArgumentType (Language.Prolog.Syntax.Var _) = Out
getArgumentType (Language.Prolog.Syntax.Cons arg1 arg2) =
    if Out `elem` [getArgumentType arg1, getArgumentType arg2]
        then Out
        else In
getArgumentType _ = error "Malformed query argument"
