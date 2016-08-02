{-# LANGUAGE FlexibleContexts #-}

module SymbolicEvaluationGraphs.Heuristic where

import Data.Implicit
import Data.Foldable (toList)
import Data.Maybe
import Data.Either.Utils
import ExprToTerm.Conversion
import Data.Rewriting.Term.Type (Term(..))
import SymbolicEvaluationGraphs.Types
import SymbolicEvaluationGraphs.InferenceRules (suc, caseRule, eval, backtrack, isBacktrackingApplicable, split)
import Diagrams.TwoD.Layout.Tree
import Data.Rewriting.Term (vars, root)

minExSteps :: Int
minExSteps = 1
maxBranchingFactor :: Int
maxBranchingFactor = 4

applyRule :: AbstractState -> Int -> BTree (AbstractState, String)
applyRule s@([],_) n = leaf (s, "") -- just for output (base case of recursion)
applyRule s@(([],_,_):_,_) n = BNode (s, "suc") (applyRule (suc s) n) Empty
applyRule s@((_,_,Nothing):_,_) n =
    BNode (s, "case") (applyRule (caseRule s) (n+1)) Empty
applyRule s n =
    if isBacktrackingApplicable s
        then BNode (s, "backtrack") (applyRule (backtrack s) n) Empty
        else if n >= minExSteps && (case s of ((t:_,_,Nothing):_,_) -> isFunctionSymbolRecursive (Fun (fromRight (root t)) [])
                                              ((_:_,_,Just clause):_,_) -> isClauseRecursive clause) then fromMaybe (case s of (([_],_,_):_,_) -> e
                                                                                                                               _ -> split s) i else e
  where
    ss = eval s
    s1 = fst ss
    s2 = snd ss
    i = case s of ((t:_,_,Nothing):_,_) ->  BNode (s, "instance") (applyRule (tryToApplyInstanceRule s (getInstanceCandidates (BNode s Empty Empty) g{-TODO add current graph to args (-> use zippers?)-})) n) Empty --TODO: terminate here
                  _ -> Nothing
    e = BNode (s, "eval") (applyRule s1 n) (applyRule s2 n)

getInstanceCandidates
    :: BTree (AbstractState, String)
    -> BTree (AbstractState, String)
    -> String
    -> [(AbstractState, String)]
getInstanceCandidates (BNode node _ _) graph =
    filter
        (\x ->
              snd x /= "instance" &&
              (getVarNum (fst node) >= getVarNum (fst x) ||
               (isFunctionSymbolRecursive (nodeHead node) &&
                branchingFactor (nodeHead node) > maxBranchingFactor)))
        (toList graph)

nodeHead :: (AbstractState, String) -> Term'
nodeHead (((t:_,_,Nothing):_,_),_) = Fun (fromRight (root t)) []
nodeHead _ = error "Abstract State is annotated with a clause."

getVarNum :: AbstractState -> Int
getVarNum (ss,_) =
    length
        (concatMap
             (\(qs,_,_) ->
                   concatMap vars qs)
             ss)

isClauseRecursive :: Clause -> Bool
isClauseRecursive (_,b) = any (\x -> isFunctionSymbolRecursive (Fun (fromRight (root x)) [])) (getMetaPredicates b)

isFunctionSymbolRecursive
    :: Implicit_ [Clause]
    => Term' -> Bool
isFunctionSymbolRecursive (Fun f _) =
    f == "repeat" ||
    ((f `notElem`
      [ "abolish"
      , "arg"
      , "=:="
      , "=\\="
      , ">"
      , ">="
      , "<"
      , "=<"
      , "asserta"
      , "assertz"
      , "at_end_of_stream"
      , "atom"
      , "atom_chars"
      , "atom_codes"
      , "atom_concat"
      , "atom_length"
      , "atomic"
      , "bagof"
      , "call"
      , "catch"
      , "char_code"
      , "char_conversion"
      , "clause"
      , "close"
      , "compound"
      , ","
      , "copy_term"
      , "current_char_conversion"
      , "current_input"
      , "current_op"
      , "current_output"
      , "current_predicate"
      , "current_prolog_flag"
      , "!"
      , ";"
      , "fail"
      , "findall"
      , "float"
      , "flush_output"
      , "functor"
      , "get_byte"
      , "get_char"
      , "get_code"
      , "halt"
      , "->"
      , "integer"
      , "is"
      , "nl"
      , "nonvar"
      , "\\+"
      , "number"
      , "number_chars"
      , "number_codes"
      , "once"
      , "op"
      , "open"
      , "peek_byte"
      , "peek_char"
      , "peek_code"
      , "put_byte"
      , "put_char"
      , "put_code"
      , "read"
      , "read_term"
      , "retract"
      , "set_input"
      , "set_output"
      , "set_prolog_flag"
      , "set_stream_position"
      , "setof"
      , "stream_property"
      , "sub_atom"
      , "@>"
      , "@>="
      , "=="
      , "@<"
      , "@=<"
      , "\\=="
      , "throw"
      , "true"
      , "\\="
      , "="
      , "unify_with_occurs_check"
      , "=.."
      , "var"
      , "write"
      , "write_canonical"
      , "write_term"
      , "writeq"]) &&
     any
         (\x ->
               isFunctionSymbolRecursiveHelper f [root (fst x)] x)
         (filter
              (\x ->
                    Right f == root (fst x))
              param_))
isFunctionSymbolRecursive _ = error "No function symbol provided."

isFunctionSymbolRecursiveHelper
    :: Implicit_ [Clause]
    => String -> [Either String String] -> Clause -> Bool
isFunctionSymbolRecursiveHelper f hrs c =
    let his =
            map
                snd
                (filter
                     (\(x,y) ->
                           root x == root (fst y))
                     (maybe [] getMetaPredicates (snd c) `cartesianProduct`
                      param_))
    in (not (null his) &&
        (any
             (\x ->
                   root (fst x) `elem` hrs)
             his ||
         any
             (\x ->
                   isFunctionSymbolRecursiveHelper f (root (fst x) : hrs) x)
             his))

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys =
    [ (x, y)
    | x <- xs
    , y <- ys ]

getMetaPredicates :: Term' -> [Term']
getMetaPredicates t@(Fun f args) =
    t :
    if f `elem` [",", ";", "->", "call", "\\+", "once"]
        then concatMap getMetaPredicates args
        else []
getMetaPredicates v = [v]

branchingFactor
    :: Implicit_ [Clause]
    => Term' -> Int
branchingFactor (Fun f _) =
    length (filter (== Right f) (map (root . fst) (param_ :: [Clause])))
branchingFactor _ = error "No function symbol provided."

tryToApplyInstanceRule :: AbstractState -> [AbstractState] -> Maybe AbstractState
tryToApplyInstanceRule s candidates = Nothing --TODO
