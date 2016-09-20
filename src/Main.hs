{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types, UndecidableInstances #-}
module Main where

import           System.Environment
import           Language.Prolog.Parser
import           Query.Utilities
import           Data.Rewriting.Substitution (Subst, GSubst, unify, apply, compose)
import qualified Data.Rewriting.Substitution.Type as Subst
import           Data.Rewriting.Term.Type                (Term (..))
import           SymbolicEvaluationGraphs.Types
import           SymbolicEvaluationGraphs.Utilities
import           SymbolicEvaluationGraphs.InferenceRules
import           SymbolicEvaluationGraphs.Visualization
import           SymbolicEvaluationGraphs.Heuristic
import           TRS.Encoding hiding (getNode)
import           TRS.TPDB
import           Data.Maybe
import           Data.Map
import qualified Data.List
import           Control.Monad.State
import qualified Text.PrettyPrint.ANSI.Leijen as P (text, putDoc, vcat)
import           Data.Rewriting.Rule.Pretty
import           Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty))

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

{-main = do
      args <- getArgs
      (exprs,_) <- parseProlog2 (head args)
      printSymbolicEvaluationGraph (head (tail args)) =<< evalStateT (generateSymbolicEvaluationGraph (head (getQueryClasses exprs))) empty-}

{-main = do
      (exprs,_) <- parseProlog2 "C:\\Users\\Philipp\\Documents\\Uni\\Bachelorarbeit\\code\\resources\\example21.pl"
      rewriteRules <- evalStateT (generateSymbolicEvaluationGraph (head (getQueryClasses exprs)) >>=
        generateRewriteRules) Data.Map.empty
      P.putDoc (P.vcat (Data.List.map (prettyRule (P.text "->") P.text P.text) rewriteRules))-}

main = do
      --let mguG = Subst.fromMap (fromList [("T29",Fun "f" [Var "T40", Var "T50"])])
      --let p = Subst.fromMap (fromList [("T29", Fun "f" [Fun "s" [Fun "s" [Fun "s" [Fun "0" []]]], Fun "0" []])])
      --error (show (Subst.toMap p `Data.Map.union` Subst.toMap (applyToSubKeys mguG p)))
      args <- getArgs
      (exprs,_) <- parseProlog2 (head args)
      putStrLn ""
      ((graph, groundnessAnalysisInformation), generalizationInformation) <- runStateT (runStateT (generateSymbolicEvaluationGraph (head (getQueryClasses exprs))) Data.Map.empty) Data.Map.empty
      if not (Data.List.null (fix (\f n ->
            case n of
                BNode (_,(s,_)) l r ->
                    [ s
                    | s == "split" ] ++
                    f l ++ f r
                Empty -> []) graph)) then do
        printSymbolicEvaluationGraph (head (tail args)) graph
        putStrLn "Please take a look at the generated graph. Are there multiplicative split nodes (y/n)? Potential nodes are marked in red."
        ans <- getLine
        putStrLn ""
        if ans == "y" || ans == "yes" then do
          putStrLn "Please indicate the positions of the multiplicative split nodes:"
          mulSplitNodes <- readLn
          putStrLn ""
          putStrLn "Please note that this will fail if the symbolic evaluation graph is not decomposable."
          putStrLn ""
          putStrLn ""
          rewriteRules <- evalStateT (evalStateT (generateRewriteRulesForGraphsWithMultSplitNodes graph mulSplitNodes) groundnessAnalysisInformation) generalizationInformation
          concatSaveFileInTPDBFormat (head (tail (tail args))) rewriteRules
        else do
          rewriteRules <- evalStateT (evalStateT (generateRewriteRules graph) groundnessAnalysisInformation) generalizationInformation
          saveFileInTPDBFormat (head (tail (tail args))) rewriteRules
      else do
        rewriteRules <- evalStateT (evalStateT (generateRewriteRules graph) groundnessAnalysisInformation) generalizationInformation
        saveFileInTPDBFormat (head (tail (tail args))) rewriteRules


getNode :: BTree (AbstractState,(String,Int)) -> Int -> AbstractState
getNode graph i = head (fix (\f n ->
      case n of
          BNode (s,(_,j)) l r ->
              if i == j then
              [s] else
              f l ++ f r
          Empty -> []) graph)

{-main = print
        (tryToApplyInstanceRule_
          ([([Fun "mult" [Var "T37",Fun "Left 0" []]],Subst.fromMap (Data.Map.fromList [("T0",Fun "s" [Var "T3"]),("T1",Var "T4"),("T3",Var "T5"),("T36",Var "T37"),("T37",Var "T37"),("T4",Fun "Left 0" []),("T5",Fun "s" [Var "T36"]),("X",Fun "mult" [Var "T37",Fun "Left 0" []]),("Z",Var "T36")]),Nothing)],([Var "T37"],[(Fun "mult" [Fun "s" [Fun "s" [Var "T37"]],Var "T1"],Fun "mult" [Fun "Left 0" [],Var "X"]),(Fun "mult" [Fun "s" [Var "T37"],Fun "Left 0" []],Fun "mult" [Fun "Left 0" [],Var "X"])]))
          [([([Fun "mult" [Var "T5",Fun "Left 0" []]],Subst.fromMap (Data.Map.fromList [("T0",Fun "s" [Var "T3"]),("T1",Var "T4"),("T3",Var "T5"),("T4",Fun "Left 0" []),("T5",Var "T5"),("X",Fun "mult" [Var "T5",Fun "Left 0" []]),("Z",Var "T3")]),Nothing)],([Var "T5"],[(Fun "mult" [Fun "s" [Var "T5"],Var "T1"],Fun "mult" [Fun "Left 0" [],Var "X"])]))]
        )-}

{-main = print
       (tryToApplyInstanceRule_
          ([([Fun "mult" [Var "T78",Fun "Left 0" []]],Subst.fromMap (Data.Map.fromList [("T0",Fun "s" [Var "T3"]),("T1",Var "T4"),("T3",Var "T5"),("T36",Var "T37"),("T37",Fun "s" [Var "T77"]),("T4",Fun "Left 0" []),("T5",Fun "s" [Var "T36"]),("T77",Var "T78"),("T78",Var "T78"),("X",Fun "mult" [Var "T78",Fun "Left 0" []]),("Z",Var "T77")]),Nothing)],([Var "T78"],[(Fun "mult" [Fun "s" [Fun "s" [Fun "s" [Var "T78"]]],Var "T1"],Fun "mult" [Fun "Left 0" [],Var "X"]),(Fun "mult" [Fun "s" [Fun "s" [Var "T78"]],Fun "Left 0" []],Fun "mult" [Fun "Left 0" [],Var "X"]),(Fun "mult" [Fun "s" [Var "T78"],Fun "Left 0" []],Fun "mult" [Fun "Left 0" [],Var "X"])]))
          [([([Fun "mult" [Var "T37",Fun "Left 0" []]],Subst.fromMap (Data.Map.fromList [("T0",Fun "s" [Var "T3"]),("T1",Var "T4"),("T3",Var "T5"),("T36",Var "T37"),("T37",Var "T37"),("T4",Fun "Left 0" []),("T5",Fun "s" [Var "T36"]),("X",Fun "mult" [Var "T37",Fun "Left 0" []]),("Z",Var "T36")]),Nothing)],([Var "T37"],[(Fun "mult" [Fun "s" [Fun "s" [Var "T37"]],Var "T1"],Fun "mult" [Fun "Left 0" [],Var "X"]),(Fun "mult" [Fun "s" [Var "T37"],Fun "Left 0" []],Fun "mult" [Fun "Left 0" [],Var "X"])]))]
        )-}

{-main = print
          (tryToApplyInstanceRule
            ([([Fun "mult" [Var "T5",Fun "Left 0" []]],Subst.fromMap (Data.Map.fromList [("T0",Fun "s" [Var "T3"]),("T1",Var "T4"),("T3",Var "T5"),("T4",Fun "Left 0" []),("T5",Var "T5"),("X",Fun "mult" [Var "T5",Fun "Left 0" []]),("Z",Var "T3")]),Nothing)],([Var "T5"],[(Fun "mult" [Fun "s" [Var "T5"],Var "T1"],Fun "mult" [Fun "Left 0" [],Var "X"])]))
            [([([Fun "mult" [Var "T0",Var "T1"]],Subst.fromMap (Data.Map.fromList []),Nothing)],([Var "T0"],[]))]
          )-}

-- main = print (isFunctionSymbolRecursive (Fun "mult" []) 2) --True
-- main = print (isFunctionSymbolRecursive (Fun "add" []) 2) --False

--main = print (showTerm' (Fun "add" [Fun "Left 0" [], Fun "s" [Fun "Left 0" []]]))

-- main = print (show (freshVariable (Var "")) ++ show (freshVariable (Var ""))) -- the two fresh variables should be distinct

--main = print (isFunctionSymbolRecursive (Fun "q" []))

{-main = print (theta `subDif` (sigma `compose` theta)) >>
       putStrLn "" >>
       print sigma
  where theta = Subst.fromMap (fromList [("T0", Fun "s" [Var "T5"]), ("T1", Var "T2")])
        sigma = Subst.fromMap (fromList [("T5", Fun "f" [Var "T7"]), ("T2", Var "T4")])-}

{-main = print (Data.Map.map (apply sigma) (Subst.toMap theta))
  where theta = Subst.fromMap (fromList [("T0", Fun "s" [Var "T5"]), ("T1", Var "T2")])
        sigma = Subst.fromMap (fromList [("T5", Fun "f" [Var "T7"]), ("T2", Var "T4")])-}

{-main = let t = Fun "k" [Fun "s" [Fun "k" [Fun "s" [Fun "s" [Fun "0" []]]]], Fun "f" [], Var "x"] in
       print (findFiniteGeneralizationPosition t)-}

{-main = let s = ([([Fun "k" [Fun "s" [Fun "k" [Fun "s" [Fun "s" [Fun "0" []]]]], Fun "f" [], Var "x"]], Subst.fromMap (fromList []), Nothing)], ([],[])) in
       print =<< evalStateT (applyGeneralizationStep s) 0-}

--main = printArrayLineByLine (getMetaPredicates (Fun "," [Var "p", Fun ";" [Fun "f" [Fun "," [Var "r", Var "r"]], Var "q"]]))

printArrayLineByLine :: Show a => [a] -> IO ()
printArrayLineByLine [] = putStrLn ""
printArrayLineByLine (x:xs) = do
  print x
  putStrLn ""
  printArrayLineByLine xs
