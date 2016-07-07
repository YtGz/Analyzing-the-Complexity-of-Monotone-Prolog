{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types, UndecidableInstances #-}
module Main where

import           Data.Rewriting.Substitution (Subst, GSubst, unify)
import qualified Data.Rewriting.Substitution.Type as Subst
import           Data.Rewriting.Term.Type                (Term (..))
import           SymbolicEvaluationGraphs.InferenceRules
import           Data.Implicit

--main = print (suc ([(Hole, "")],([AVar "T1", AVar "T2"],[])))

--main = print (exprToClause (Op ":-" [Str "vowel" [Var "X"],Str "member" [Var "X",Cons (Str "a" []) (Cons (Str "e" []) (Cons (Str "i" []) (Cons (Str "o" []) (Cons (Str "u" []) (Str "[]" [])))))]]))

-- main = print (exprToClause (Str "nr_vowel" [Str "[]" [],Num (Left 0)]))

--main = print (root (Str "nr_vowel" [Str "[]" [],Num (Left 0)]))

{-main = do
    as <- getArgs
    mapM_ parseProc as-}

{-main = do
  as <- getArgs
  (exprs,_) <- parseProlog2 (head as)
  print (getQueryClasses exprs)-}

{-main = do
  as <- getArgs
  (exprs,_) <- parseProlog2 (head as)
  print (map exprToClause exprs)-}

{-main = do
  as <- getArgs
  (exprs,_) <- parseProlog2 (head as)
  let clauses = map termToClause (map exprToTerm (filter (not . isQuery) exprs))
  print (caseRule clauses ([(Term (Fun "add" [Fun "0" [], Fun "s" [Fun "0" []]]), Subst.fromMap (Data.Map.fromList [("X", Data.Rewriting.Term.Type.Var "X")]) ,Nothing)],([],[])))-}

--main = print (unify' (Fun "star" [Var "T1", Var "T2"]) (Fun "star" [Var "XS", Fun "[]" []]))

 {-main = do
  as <- getArgs
  (exprs,_) <- parseProlog2 (head as)
  let clauses = map termToClause (map exprToTerm (filter (not . isQuery) exprs))
  print (eval clauses ([(Term (Fun "add" [Fun "0" [], Fun "s" [Fun "0" []]]), Subst.fromMap (Data.Map.fromList [("X", Data.Rewriting.Term.Type.Var "X")]) ,Nothing)],([],[])))-}

main = print test

test :: Implicit_ [Clause] => [Clause]
test = param_
