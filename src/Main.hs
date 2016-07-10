{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types, UndecidableInstances #-}
module Main where

import           Data.Rewriting.Substitution (Subst, GSubst, unify, apply)
import qualified Data.Rewriting.Substitution.Type as Subst
import           Data.Rewriting.Term.Type                (Term (..))
import           SymbolicEvaluationGraphs.InferenceRules
import           Data.Implicit
import           Data.Map

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

-- main = print (caseRule test ([(Term (Fun "add" [Fun "Left 0" [], Fun "s" [Fun "Left 0" []]]), Subst.fromMap (Data.Map.fromList [("X", Data.Rewriting.Term.Type.Var "X")]), Nothing)],([],[])))

main = let (Just mgu) = unify' (Fun "star" [Var "T1", Var "T2"]) (Fun "star" [Var "XS", Fun "[]" []]) in print (apply mgu (Fun "star" [Var "T1", Var "T2"]))


--main = print (unify' (Fun "add" [Var "X", Fun "s" [Var "Y"]]) (Fun "add" [Var "X", Fun "s" [Var "Y"]]))           -- replaces initial variables with initial (fresh) abstract variables
--main = print (eval test (caseRule test ([(Term (Fun "add" [Fun "Left 0" [], Fun "s" [Fun "Left 0" []]]), Subst.fromMap (Data.Map.fromList [("X", Data.Rewriting.Term.Type.Var "X")]), Nothing)],([],[]))))

-- main = print test

test :: Implicit_ [Clause] => [Clause]
test = param_
