module Query.Utilities
  ( isQuery,
    ArgumentType (In, Out),
    FunctorName,
    QueryClass,
    getQueryClass,
    getQueryClasses )
where

import qualified Language.Prolog.Syntax
import qualified Data.List

isQuery :: Language.Prolog.Syntax.Expr -> Bool
isQuery (Language.Prolog.Syntax.Op ":-" [_]) = True
isQuery _ = False


------------------------------------------------------------------------------
-- query classes

data ArgumentType = In | Out deriving (Show, Eq)
type FunctorName = String
type QueryClass = (FunctorName, [ArgumentType])

getQueryClasses :: [Language.Prolog.Syntax.Expr] -> [QueryClass]
getQueryClasses exprs = Data.List.nub (map getQueryClass (filter isQuery exprs))

getQueryClass :: Language.Prolog.Syntax.Expr -> QueryClass
getQueryClass (Language.Prolog.Syntax.Op _ [Language.Prolog.Syntax.Str functor args]) =
   (functor, map getArgumentType args)

getArgumentType :: Language.Prolog.Syntax.Expr -> ArgumentType
getArgumentType (Language.Prolog.Syntax.Str _ _) = In
getArgumentType (Language.Prolog.Syntax.Num _) = In
getArgumentType (Language.Prolog.Syntax.Var _) = Out
getArgumentType _ = error "Malformed query argument"
