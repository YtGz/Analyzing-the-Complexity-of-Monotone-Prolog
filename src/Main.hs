{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types, UndecidableInstances #-}
module Main where

import           System.Environment
import           Language.Prolog.Parser
import           Query.Utilities
import           Data.Rewriting.Substitution (Subst, GSubst, unify, apply)
import qualified Data.Rewriting.Substitution.Type as Subst
import           Data.Rewriting.Term.Type                (Term (..))
import           SymbolicEvaluationGraphs.Types
import           SymbolicEvaluationGraphs.Utilities
import           SymbolicEvaluationGraphs.InferenceRules
import           SymbolicEvaluationGraphs.Visualization
import           SymbolicEvaluationGraphs.Heuristic
import           Data.Maybe
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

-- main = print (apply (fromJust (unify' (Fun "star" [Var "T1", Var "T2"]) (Fun "star" [Var "XS", Fun "[]" []]))) (Fun "star" [Var "T1", Var "T2"]))

-- main = print (apply (fromJust (unify' input input)) input)          -- replaces initial variables with initial (fresh) abstract variables
       -- where input = Fun "add" [Var "X", Fun "s" [Var "Y"]]

-- main = print (eval (caseRule ([(Term (Fun "add" [Fun "Left 0" [], Fun "s" [Fun "Left 0" []]]), Subst.fromMap (Data.Map.fromList []), Nothing)],([],[]))))

{-main = do
  --as <- getArgs
  (exprs,_) <- parseProlog2 "C:\\Users\\Philipp\\Documents\\Uni\\Bachelorarbeit\\code\\resources\\add_mul.pl" --(head as)
  let o = getInitialAbstractState (head (getQueryClasses exprs))
      o' = caseRule o
      os = eval o'
      o1 = caseRule (fst os)
      o1' = backtrack o1
      o2 = eval (snd os)
      o3 = caseRule (fst o2)
      in do
        print o
        putStrLn ""
        print o'
        putStrLn ""
        print os
        putStrLn ""
        putStrLn ""
        print (fst os)
        putStrLn ""
        print o1
        putStrLn ""
        print o1'
        putStrLn ""
        putStrLn ""
        putStrLn ""
        print (snd os)
        putStrLn ""
        print o2
        putStrLn ""
        putStrLn ""
        print (fst o2)
        putStrLn ""
        print o3
        putStrLn ""
        putStrLn ""
        print (snd o2)-}

{-main = do
      (exprs,_) <- parseProlog2 "C:\\Users\\Philipp\\Documents\\Uni\\Bachelorarbeit\\code\\resources\\add_mul.pl"
      let o = getInitialAbstractState (head (getQueryClasses exprs)) in
        printSymbolicEvaluationGraph (applyRules o)-}

main = do
      (exprs,_) <- parseProlog2 "C:\\Users\\Philipp\\Documents\\Uni\\Bachelorarbeit\\code\\resources\\add_mul.pl"
      let o = getInitialAbstractState (head (getQueryClasses exprs)) in
        printSymbolicEvaluationGraph (generateSymbolicEvaluationGraph o)

-- main = print (isFunctionSymbolRecursive (Fun "mult" []) 2) --True
-- main = print (isFunctionSymbolRecursive (Fun "add" []) 2) --False

--main = print (showTerm' (Fun "add" [Fun "Left 0" [], Fun "s" [Fun "Left 0" []]]))

-- main = print (show (freshVariable (Var "")) ++ show (freshVariable (Var ""))) -- the two fresh variables should be distinct

--main = print (isFunctionSymbolRecursive (Fun "q" []))

--main = printArrayLineByLine (getMetaPredicates (Fun "," [Var "p", Fun ";" [Fun "f" [Fun "," [Var "r", Var "r"]], Var "q"]]))

printArrayLineByLine :: Show a => [a] -> IO ()
printArrayLineByLine [] = putStrLn ""
printArrayLineByLine (x:xs) = do
  print x
  putStrLn ""
  printArrayLineByLine xs
