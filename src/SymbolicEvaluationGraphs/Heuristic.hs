{-# LANGUAGE FlexibleContexts #-}

module SymbolicEvaluationGraphs.Heuristic where

import Data.Implicit
import Data.Foldable (toList)
import Data.Maybe
import Data.List (find, nubBy, nub, (\\))
import Control.Arrow ((***))
import Data.Either.Utils
import Data.Tree.Zipper
import ExprToTerm.Conversion
import Data.Rewriting.Term (vars)
import Data.Rewriting.Term.Type (Term(..))
import Data.Rewriting.Substitution (unify, apply)
import Data.Rewriting.Substitution.Type (toMap, fromMap)
import SymbolicEvaluationGraphs.Types
import SymbolicEvaluationGraphs.InferenceRules
       (suc, caseRule, eval, backtrack, isBacktrackingApplicable, split,
        tryToApplyInstanceRule, parallel, unify', arityOfRootSymbol)
import Data.Tree
import Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty))
import qualified Data.Rewriting.Term (root)

minExSteps :: Int
minExSteps = 1

maxBranchingFactor :: Int
maxBranchingFactor = 4

generateSymbolicEvaluationGraph :: AbstractState
                                -> BTree (AbstractState, String)
generateSymbolicEvaluationGraph initialState =
    roseTreeToBTree
        (toTree (applyRule (fromTree (Node (initialState, "") [])) 0))

type TreePath a = [TreePos Full a -> TreePos Full a]

firstChild' = fromJust . firstChild

parent' = fromJust . parent

prev' = fromJust . prev

next' = fromJust . next

-- Determine the path from the root of the tree to the cursor.
pathToMe
    :: TreePos Full a -> TreePath a
pathToMe t
  | isRoot t = []
  | isFirst t = firstChild' : pathToMe (parent' t)
  | otherwise = next' : pathToMe (prev' t)

followThePath :: Tree a -> TreePath a -> TreePos Full a
followThePath t = foldr ($) (fromTree t)

applyRule
    :: TreePos Full (AbstractState, String)
    -> Int
    -> TreePos Full (AbstractState, String)
applyRule tp n =
    let s = fst (label tp)
    in case s of
           ([],_) ->
               fst
                   (insertAndMoveToChild
                        (modifyLabel
                             (\(x,_) ->
                                   (x, ""))
                             tp)
                        (Nothing, Nothing))
           (([],_,_):_,_) ->
               applyRule
                   (fst
                        (insertAndMoveToChild
                             (modifyLabel
                                  (\(x,_) ->
                                        (x, "suc"))
                                  tp)
                             (Just (suc s, ""), Nothing)))
                   n
           _
             | isJust
                  ((\(_,_,x) ->
                         x)
                       (head (fst s))) &&
                   isBacktrackingApplicable s ->
                 applyRule
                     (fst
                          (insertAndMoveToChild
                               (modifyLabel
                                    (\(x,_) ->
                                          (x, "backtrack"))
                                    tp)
                               (Just (backtrack s, ""), Nothing)))
                     n
             | n >= minExSteps &&
                   (case s of
                        ((t:_,_,Nothing):_,_) ->
                            (\x ->
                                  case x of
                                      (Right _) -> True
                                      _ -> False)
                                (Data.Rewriting.Term.root t) &&
                            isFunctionSymbolRecursive
                                (Fun
                                     (fromRight (Data.Rewriting.Term.root t))
                                     [])
                                (arityOfRootSymbol t)
                        ((_:_,_,Just clause):_,_) -> isClauseRecursive clause) ->
                 if length (fst s) > 1
                     then par
                     else fromMaybe
                              (case s of
                                   (([_],_,Nothing):_,_) -> c
                                   (([_],_,_):_,_) -> e
                                   _ -> sp)
                              i
             | otherwise ->
                 case s of
                     ((_,_,Nothing):_,_) -> c
                     _ -> e
               where ss = eval s
                     s0 = fst ss
                     s1 = snd ss
                     sps = split s
                     sp0 = fst sps
                     sp1 = snd sps
                     pars = parallel s
                     par0 = fst pars
                     par1 = snd pars
                     i =
                         case s of
                             ((t:_,_,Nothing):_,_) ->
                                 let inst =
                                         tryToApplyInstanceRule
                                             s
                                             (map
                                                  fst
                                                  (getInstanceCandidates
                                                       (label tp)
                                                       (roseTreeToBTree
                                                            (toTree tp))))
                                 in if isJust inst
                                        then Just
                                                 (fst
                                                      (insertAndMoveToChild
                                                           (modifyLabel
                                                                (\(x,_) ->
                                                                      ( x
                                                                      , "instance"))
                                                                tp)
                                                           ( Just
                                                                 ( fromJust
                                                                       inst
                                                                 , "")
                                                           , Nothing))) {-TODO: terminate here-}
                                        else Nothing
                             _ -> Nothing
                     b0 x y =
                         fromMaybe
                             (error "1")
                             (parent
                                  (insert
                                       (tree
                                            ((fromMaybe (error "2") .
                                              firstChild)
                                                 (followThePath
                                                      (tree
                                                           (root
                                                                (applyRule
                                                                     (fst y)
                                                                     n)))
                                                      (pathToMe x))))
                                       (first (children x))))
                     b1 x y =
                         fromMaybe
                             (error "4")
                             (parent
                                  (insert
                                       (tree
                                            ((fromMaybe (error "5") . lastChild)
                                                 (followThePath
                                                      (tree
                                                           (root
                                                                (applyRule
                                                                     (snd y)
                                                                     n)))
                                                      (pathToMe x))))
                                       (Data.Tree.Zipper.last (children x))))
                     cs0 =
                         insertAndMoveToChild tp (Just (s0, ""), Just (s1, ""))
                     cs1 =
                         insertAndMoveToChild
                             tp
                             (Just (sp0, ""), Just (sp1, ""))
                     cs2 =
                         insertAndMoveToChild
                             tp
                             (Just (par0, ""), Just (par1, ""))
                     e =
                         b1
                             (b0
                                  (modifyLabel
                                       (\(x,_) ->
                                             (x, "eval"))
                                       tp)
                                  cs0)
                             cs0
                     sp =
                         b1
                             (b0
                                  (modifyLabel
                                       (\(x,_) ->
                                             (x, "split"))
                                       tp)
                                  cs1)
                             cs1
                     par =
                         b1
                             (b0
                                  (modifyLabel
                                       (\(x,_) ->
                                             (x, "parallel"))
                                       tp)
                                  cs2)
                             cs2
                     c =
                         applyRule
                             (fst
                                  (insertAndMoveToChild
                                       (modifyLabel
                                            (\(x,_) ->
                                                  (x, "case"))
                                            tp)
                                       (Just (caseRule s, ""), Nothing)))
                             (n + 1)

{-applyRule s@(([],_,_):_,_) n = BNode (s, "suc") (applyRule (suc s) n) Empty
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
    e = BNode (s, "eval") (applyRule s1 n) (applyRule s2 n)-}
insertAndMoveToChild
    :: TreePos Full (AbstractState, String)
    -> (Maybe (AbstractState, String), Maybe (AbstractState, String))
    -> (TreePos Full (AbstractState, String), TreePos Full (AbstractState, String))
insertAndMoveToChild tp (l,r) =
    if not (isLeaf tp)
        then error "Can only insert a new element at a leaf of the tree."
        else ( if isJust l
                   then fromMaybe (error "7") (firstChild newTp)
                   else newTp
             , if isJust r
                   then fromMaybe (error "8") (lastChild newTp)
                   else newTp)
  where
    newTp =
        modifyTree
            (\(Node n []) ->
                  Node
                      n
                      (maybe
                           []
                           ((: []) .
                            (\x ->
                                  Node x []))
                           l ++
                       maybe
                           []
                           ((: []) .
                            (\x ->
                                  Node x []))
                           r))
            tp

getInstanceCandidates
    :: (AbstractState, String)
    -> BTree (AbstractState, String)
    -> [(AbstractState, String)]
getInstanceCandidates node graph =
    filter
        (\x ->
              snd x /= "instance" &&
              (getVarNum (fst node) >= getVarNum (fst x) ||
               ((\x ->
                      case x of
                          (Right _) -> True
                          _ -> False)
                    (Data.Rewriting.Term.root
                         ((\(((t:_,_,_):_,_),_) ->
                                t)
                              node)) &&
                isFunctionSymbolRecursive
                    (nodeHead node)
                    (arityOfRootSymbol
                         ((\(((t:_,_,Nothing):_,_),_) ->
                                t)
                              node)) &&
                branchingFactor (nodeHead node) > maxBranchingFactor)))
        (toList graph)

nodeHead :: (AbstractState, String) -> Term'
nodeHead (((t:_,_,Nothing):_,_),_) =
    Fun (fromRight (Data.Rewriting.Term.root t)) []
nodeHead _ = error "Abstract State is annotated with a clause."

getVarNum :: AbstractState -> Int
getVarNum (ss,_) =
    length
        (concatMap
             (\(qs,_,_) ->
                   concatMap Data.Rewriting.Term.vars qs)
             ss)

isClauseRecursive :: Clause -> Bool
isClauseRecursive (_,Just b) =
    any
        (\x ->
              (\x ->
                    case x of
                        (Right _) -> True
                        _ -> False)
                  (Data.Rewriting.Term.root x) &&
              isFunctionSymbolRecursive
                  (Fun (fromRight (Data.Rewriting.Term.root x)) [])
                  (arityOfRootSymbol x))
        (getMetaPredications b)

isFunctionSymbolRecursive
    :: Implicit_ [Clause]
    => Term' -> Int -> Bool
isFunctionSymbolRecursive (Fun f _) arity =
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
               isFunctionSymbolRecursiveHelper
                   f
                   [Data.Rewriting.Term.root (fst x)]
                   x)
         (mapMaybe
              (\x ->
                    let startF = Fun f (map (Var . show) (take arity [1,2 ..]))
                        sub = unify' startF (fst x)
                    in if isJust sub
                           then Just
                                    ((apply (fromJust sub) ***
                                      fmap (apply (fromJust sub)))
                                         x)
                           else Nothing)
              (filter
                   (\x ->
                         Right f == Data.Rewriting.Term.root (fst x))
                   param_)))
isFunctionSymbolRecursive _ _ = error "No function symbol provided."

isFunctionSymbolRecursiveHelper
    :: Implicit_ [Clause]
    => String -> [Either String String] -> Clause -> Bool
isFunctionSymbolRecursiveHelper f hrs c =
    let his =
            mapMaybe
                (\(x,y) ->
                      let sub = unify' x (fst y)
                      in if isJust sub
                             then Just
                                      ((apply (fromJust sub) ***
                                        fmap (apply (fromJust sub)))
                                           y)
                             else Nothing)
                (filter
                     (\(x,y) ->
                           Data.Rewriting.Term.root x ==
                           Data.Rewriting.Term.root (fst y))
                     (maybe [] getMetaPredications (snd c) `cartesianProduct`
                      param_))
    in (not (null his) &&
        (any
             (\x ->
                   Data.Rewriting.Term.root (fst x) `elem` hrs)
             his ||
         any
             (\x ->
                   isFunctionSymbolRecursiveHelper
                       f
                       (Data.Rewriting.Term.root (fst x) : hrs)
                       x)
             his))

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys =
    [ (x, y)
    | x <- xs
    , y <- ys ]

getMetaPredications :: Term' -> [Term']
getMetaPredications t@(Fun f args) =
    t :
    if f `elem` [",", ";", "->", "call", "\\+", "once"]
        then concatMap getMetaPredications args
        else []
getMetaPredications v = [v]

branchingFactor
    :: Implicit_ [Clause]
    => Term' -> Int
branchingFactor (Fun f _) =
    length
        (filter
             (== Right f)
             (map (Data.Rewriting.Term.root . fst) (param_ :: [Clause])))
branchingFactor _ = error "No function symbol provided."

bTreeToRoseTree :: BTree a -> Tree a
bTreeToRoseTree (BNode e Empty Empty) = Node e []
bTreeToRoseTree (BNode e l Empty) = Node e [bTreeToRoseTree l]
bTreeToRoseTree (BNode e l r) = Node e [bTreeToRoseTree l, bTreeToRoseTree r]

roseTreeToBTree :: Tree a -> BTree a
roseTreeToBTree (Node e []) = BNode e Empty Empty
roseTreeToBTree (Node e [l]) = BNode e (roseTreeToBTree l) Empty
roseTreeToBTree (Node e [l,r]) =
    BNode e (roseTreeToBTree l) (roseTreeToBTree r)
