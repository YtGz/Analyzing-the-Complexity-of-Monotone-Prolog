{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types, UndecidableInstances #-}
module Main where

import qualified Data.Map
import           Data.Rewriting.Substitution (Subst, GSubst, unify)
import qualified Data.Rewriting.Substitution.Type as Subst
import           Data.Rewriting.Term.Type                (Term (..))
import           ExprToTerm.Conversion
import           Language.Prolog.Parser
import qualified Language.Prolog.Syntax
import           SymbolicEvaluationGraphs.InferenceRules
import           System.Environment
import           Data.Default.Class
import           Data.Implicit
import           Data.Reflection hiding (D)
import           Data.Proxy
import qualified Data.List
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

--make program clauses available to every function as implicit parameter

-- values in our dynamically-constructed 'Default' over 'a'
newtype D a s = D { runD :: a }

-- a dictionary describing a 'Default'
data Default_ a = Default_ { def_ :: a }

instance Reifies s (Default_ a) => Default (D a s) where
  def = a where a = D $ def_ (reflect a)

withDefault :: a -> (forall s. Reifies s (Default_ a) => D a s) -> a
withDefault d v = reify (Default_ d) (runD . asProxyOf v)

asProxyOf :: f s -> Proxy s -> f s
asProxyOf a _ = a

--instance Default [Clause] where def = []

test :: Implicit_ [Clause] => [Clause]
test = param_

--test :: (?clauses::[Clause]) => [Clause]
--test = ?clauses

parseProc filename = do { (exprs,_) <- parseProlog2 filename
   ; putStrLn "queries: "
   ; print exprs
   } `catchIOError` \e -> putStr ("FAILED " ++ filename ++ "\n");

isQuery :: Language.Prolog.Syntax.Expr -> Bool
isQuery (Language.Prolog.Syntax.Op ":-" [_]) = True
isQuery _ = False

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
