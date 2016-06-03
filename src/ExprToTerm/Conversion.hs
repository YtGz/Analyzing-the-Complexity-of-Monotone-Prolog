module ExprToTerm.Conversion(
    Term',
    Subst',
    exprToTerm
) where

import Language.Prolog.Syntax
import Data.Rewriting.Term.Type(Term(..))
import Data.Rewriting.Substitution (Subst, GSubst)
import qualified Data.Rewriting.Substitution.Type as Subst

type Term' = Term String String

type Subst' = Subst String String

exprToTerm :: Expr -> Term'
exprToTerm (Language.Prolog.Syntax.Var s) = Data.Rewriting.Term.Type.Var s
exprToTerm (Str s exprs) = Fun s (map exprToTerm exprs)
exprToTerm (Op s exprs) = Fun s (map exprToTerm exprs)
exprToTerm (Cons e1 e2) = Fun ":" [exprToTerm e1, exprToTerm e2]
exprToTerm (Num n) = Fun (show n) []
