module Main where

import           Language.Prolog.Parser
import           Language.Prolog.Syntax
import           SymbolicEvaluationGraphs.InferenceRules
import           System.Environment

import           Data.List
import           System.IO.Error

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

main = do
  as <- getArgs
  (exprs,_) <- parseProlog2 (head as)
  let clauses = map exprToClause (filter (not . isQuery) exprs)
  print (caseRule clauses ([(Term (Str "add" [Num (Left 0),Str "s" [Num (Left 0)]]),"",Nothing)],([],[])))

parseProc filename = do { (exprs,_) <- parseProlog2 filename
   ; putStrLn "queries: "
   ; print exprs
   } `catchIOError` \e -> putStr ("FAILED " ++ filename ++ "\n");



isQuery :: Expr -> Bool
isQuery (Op ":-" [_]) = True
isQuery _ = False

data ArgumentType = In | Out deriving (Show, Eq)
type FunctorName = String
type QueryClass = (FunctorName, [ArgumentType])

getQueryClasses :: [Expr] -> [QueryClass]
getQueryClasses exprs = nub (map getQueryClass (filter isQuery exprs))

getQueryClass :: Expr -> QueryClass
getQueryClass (Op _ [Str functor args]) =
   (functor, map getArgumentType args)

getArgumentType :: Expr -> ArgumentType
getArgumentType (Str _ _) = In
getArgumentType (Num _) = In
getArgumentType (Var _) = Out
getArgumentType _ = error "Malformed query argument"
