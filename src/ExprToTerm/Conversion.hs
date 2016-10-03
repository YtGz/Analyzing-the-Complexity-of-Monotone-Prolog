{-# OPTIONS_GHC -Wall #-}
module ExprToTerm.Conversion(
    Term',
    Subst',
    Rule',
    exprToTerm
) where

import Language.Prolog.Syntax
import Data.Rewriting.Term.Type(Term(..))
import Data.Rewriting.Substitution (Subst)
import Data.Rewriting.Rule(Rule)

type Term' = Term String String

type Subst' = Subst String String

type Rule' = Rule String String

exprToTerm :: Expr -> Term'
exprToTerm (Language.Prolog.Syntax.Var s) = Data.Rewriting.Term.Type.Var s
exprToTerm (Str s exprs) = Fun s (map exprToTerm exprs)
exprToTerm (Op s exprs) = Fun s (map exprToTerm exprs)
exprToTerm (Cons e1 e2) = Fun ":" [exprToTerm e1, exprToTerm e2]
exprToTerm (Num n) = Fun (show n) []
