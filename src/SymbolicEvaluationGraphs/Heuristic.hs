{-# LANGUAGE FlexibleContexts #-}

module SymbolicEvaluationGraphs.Heuristic where

import Data.Implicit
import Data.Foldable (toList)
import Data.Maybe
import Data.List (find, nubBy, nub, (\\))
import Data.Map (fromList)
import Control.Arrow ((***))
import Control.Monad.State
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Morph
import Control.Monad.Supply
import Data.Either.Utils
import Data.Tree.Zipper
import ExprToTerm.Conversion
import Query.Utilities
import Data.Rewriting.Term (vars)
import Data.Rewriting.Term.Type (Term(..))
import Data.Rewriting.Substitution (unify, apply)
import Data.Rewriting.Substitution.Type (fromMap)
import SymbolicEvaluationGraphs.Types
import SymbolicEvaluationGraphs.InferenceRules
       (suc, caseRule, eval, backtrack, isBacktrackingApplicable, split,
        tryToApplyInstanceRule, parallel, unify', arityOfRootSymbol)
import SymbolicEvaluationGraphs.Utilities (freshVariable)
import Data.Tree
import Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty))
import qualified Data.Rewriting.Term (root)

minExSteps :: Int
minExSteps = 1

maxBranchingFactor :: Int
maxBranchingFactor = 4

generateSymbolicEvaluationGraph :: QueryClass
                                -> IO (BTree (AbstractState, (String, Int)))
generateSymbolicEvaluationGraph queryClass = do
    tp <-
        evalSupplyT
            (evalStateT
                 (do initialState <- getInitialAbstractState queryClass
                     applyRule
                         (return (fromTree (Node (initialState, ("", 0)) [])))
                         0)
                 0)
            [0 ..]
    return (roseTreeToBTree (toTree tp))

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

getInitialAbstractState
    :: (Monad m)
    => QueryClass -> Control.Monad.State.StateT Int m AbstractState
getInitialAbstractState (f,m) = do
    (vs,gs) <-
        foldr
            (\x s -> do
                 (vs,gs) <- s
                 v <- freshVariable
                 case x of
                     In -> return (v : vs, v : gs)
                     Out -> return (v : vs, gs))
            (return ([], []))
            m
    return ([([Fun f vs], fromMap (fromList []), Nothing)], (gs, []))

applyRule
    :: IO (TreePos Full (AbstractState, (String, Int)))
    -> Int
    -> Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int IO) (TreePos Full (AbstractState, (String, Int)))
applyRule ioTp n = do
    tp <- Control.Monad.State.liftIO ioTp
    let s = fst (label tp)
    let ss =
            eval s :: Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int IO) (AbstractState, AbstractState)
        s0 = do
            e <- ss
            return (fst e)
        s1 = do
            e <- ss
            return (snd e)
        sps = split s
        sp0 = do
            sps' <- sps
            return (fst sps')
        sp1 = do
            sps' <- sps
            return (snd sps')
        pars = parallel s
        par0 = fst pars
        par1 = snd pars
        i =
            case s of
                ((t:_,_,_):_,_) -> do
                    j <-
                        supply :: Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int IO) Int
                    let newTp =
                            modifyLabel
                                (\(x,_) ->
                                      (x, ("instance", j)))
                                tp
                    instanceCandidates <-
                        hoist
                            generalize
                            (getInstanceCandidates
                                 (label tp)
                                 (roseTreeToBTree (toTree newTp)))
                    let inst = tryToApplyInstanceRule s instanceCandidates
                    if isJust inst
                        then return
                                 (Just
                                      (fst
                                           (insertAndMoveToChild
                                                newTp
                                                (inst, Nothing))))
                        else return Nothing
                _ -> return Nothing
        b0 x' y = do
            x <- x'
            nT <- applyRule (return (fst y)) n
            return
                (fromJust
                     (parent
                          (insert
                               (tree
                                    ((fromJust . firstChild)
                                         (followThePath
                                              (tree (root nT))
                                              (pathToMe x))))
                               (first (children x)))))
        b1 x y = do
            nT <- applyRule (return y) n
            return
                (fromJust
                     (parent
                          (insert
                               (tree
                                    ((fromJust . lastChild)
                                         (followThePath
                                              (tree (root nT))
                                              (pathToMe x))))
                               (Data.Tree.Zipper.last (children x)))))
        cs0 = do
            s0' <- s0
            s1' <- s1
            return
                (insertAndMoveToChild
                     tp
                     (Just (s0', ("", -1)), Just (s1', ("", -1))))
        cs1 = do
            sp0' <- sp0
            sp1' <- sp1
            return
                (insertAndMoveToChild
                     tp
                     (Just (sp0', ("", -1)), Just (sp1', ("", -1))))
        cs2 =
            insertAndMoveToChild
                tp
                (Just (par0, ("", -1)), Just (par1, ("", -1)))
        e = do
            cs0' <- cs0
            j <-
                supply :: Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int IO) Int
            l <-
                b0
                    (return
                         (modifyLabel
                              (\(x,_) ->
                                    (x, ("eval", j)))
                              tp))
                    cs0'
            let tp =
                    insert
                        (tree (snd cs0'))
                        (Data.Tree.Zipper.last (children l))
            b1 l tp
        sp = do
            cs1' <- cs1
            j <-
                supply :: Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int IO) Int
            l <-
                b0
                    (return
                         (modifyLabel
                              (\(x,_) ->
                                    (x, ("split", j)))
                              tp))
                    cs1'
            let tp =
                    insert
                        (tree (snd cs1'))
                        (Data.Tree.Zipper.last (children l))
            b1 l tp
        par = do
            j <-
                supply :: Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int IO) Int
            l <-
                b0
                    (return
                         (modifyLabel
                              (\(x,_) ->
                                    (x, ("parallel", j)))
                              tp))
                    cs2
            let tp =
                    insert
                        (tree (snd cs2))
                        (Data.Tree.Zipper.last (children l))
            b1 l tp
        c = do
            j <-
                supply :: Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int IO) Int
            applyRule
                (return
                     (fst
                          (insertAndMoveToChild
                               (modifyLabel
                                    (\(x,_) ->
                                          (x, ("case", j)))
                                    tp)
                               (Just (caseRule s, ("", -1)), Nothing))))
                (n + 1)
    case s of
        ([],_) ->
            return
                (fst
                     (insertAndMoveToChild
                          (modifyLabel
                               (\(x,_) ->
                                     (x, ("", -1)))
                               tp)
                          (Nothing, Nothing)))
        (([],_,_):_,_) -> do
            j <-
                supply :: Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int IO) Int
            applyRule
                (return
                     (fst
                          (insertAndMoveToChild
                               (modifyLabel
                                    (\(x,_) ->
                                          (x, ("suc", j)))
                                    tp)
                               (Just (suc s, ("", -1)), Nothing))))
                n
        _ ->
            if isJust
                   ((\(_,_,x) ->
                          x)
                        (head (fst s))) &&
               isBacktrackingApplicable s
                then do
                    j <-
                        supply :: Control.Monad.State.StateT Int (Control.Monad.Supply.SupplyT Int IO) Int
                    applyRule
                        (return
                             (fst
                                  (insertAndMoveToChild
                                       (modifyLabel
                                            (\(x,_) ->
                                                  (x, ("backtrack", j)))
                                            tp)
                                       (Just (backtrack s, ("", -1)), Nothing))))
                        n
                else do
                    b <-
                        case s of
                            ((t:_,_,Nothing):_,_) ->
                                if case Data.Rewriting.Term.root t of
                                       (Left _) -> True
                                       _ -> False
                                    then return False
                                    else hoist
                                             generalize
                                             (isFunctionSymbolRecursive
                                                  (Fun
                                                       (fromRight
                                                            (Data.Rewriting.Term.root
                                                                 t))
                                                       [])
                                                  (arityOfRootSymbol t))
                            ((_:_,_,Just clause):_,_) ->
                                hoist generalize (isClauseRecursive clause)
                    if n >= minExSteps && b
                        then if length (fst s) > 1
                                 then par
                                 else do
                                     i' <- i
                                     maybe
                                         (case s of
                                              (([_],_,Nothing):_,_) -> c
                                              (([_],_,_):_,_) -> e
                                              _ -> sp)
                                         return
                                         i'
                        else case s of
                                 ((_,_,Nothing):_,_) -> c
                                 _ -> e

insertAndMoveToChild
    :: TreePos Full (AbstractState, (String, Int))
    -> (Maybe (AbstractState, (String, Int)), Maybe (AbstractState, (String, Int)))
    -> (TreePos Full (AbstractState, (String, Int)), TreePos Full (AbstractState, (String, Int)))
insertAndMoveToChild tp (l,r) =
    if not (isLeaf tp)
        then error "Can only insert a new element at a leaf of the tree."
        else ( if isJust l
                   then fromJust (firstChild newTp)
                   else newTp
             , if isJust r
                   then fromJust (lastChild newTp)
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
    :: (AbstractState, (String, Int))
    -> BTree (AbstractState, (String, Int))
    -> Control.Monad.State.State Int [(AbstractState, (String, Int))]
getInstanceCandidates node graph = do
    isRecursive <- 
        if isNothing
               ((\(_,_,x) ->
                      x)
                    (head (fst (fst node)))) &&
           (\x ->
                 case x of
                     (Right _) -> True
                     _ -> False)
               (Data.Rewriting.Term.root
                    ((\(((t:_,_,_):_,_),_) ->
                           t)
                         node))
            then isFunctionSymbolRecursive
                     (nodeHead node)
                     (arityOfRootSymbol
                          ((\(((t:_,_,Nothing):_,_),_) ->
                                 t)
                               node))
            else return False
    return
        (filter
             (\x ->
                   fst (snd x) /= "instance" &&
                   (getVarNum (fst node) >= getVarNum (fst x) ||
                    (isRecursive &&
                     branchingFactor (nodeHead node) > maxBranchingFactor)))
             (toList graph))

nodeHead :: (AbstractState, (String, Int)) -> Term'
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

isClauseRecursive :: Clause -> Control.Monad.State.State Int Bool
isClauseRecursive (_,Nothing) = return False
isClauseRecursive (_,Just b) =
    (fmap or .
     mapM --anyM
         (\x ->
               if case Data.Rewriting.Term.root x of
                      (Left _) -> True
                      _ -> False
                   then return False
                   else isFunctionSymbolRecursive
                            (Fun (fromRight (Data.Rewriting.Term.root x)) [])
                            (arityOfRootSymbol x)))
        (getMetaPredications b)

isFunctionSymbolRecursive
    :: Implicit_ [Clause]
    => Term' -> Int -> Control.Monad.State.State Int Bool
isFunctionSymbolRecursive (Fun f _) arity =
    if f == "repeat" ||
       f `notElem`
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
       , "writeq"]
        then do
            clauses <-
                mapMaybeM
                    (\x -> do
                         let startF =
                                 Fun f (map (Var . show) (take arity [1,2 ..]))
                         sub <- unify' startF (fst x) --TODO: save state at beginning of this function and restore it at the end
                         if isJust sub
                             then return
                                      (Just
                                           ((apply (fromJust sub) ***
                                             fmap (apply (fromJust sub)))
                                                x))
                             else return Nothing)
                    (filter
                         (\x ->
                               Right f == Data.Rewriting.Term.root (fst x))
                         param_)
            (fmap or .
             mapM --anyM
                 (\x ->
                       isFunctionSymbolRecursive_
                           f
                           [Data.Rewriting.Term.root (fst x)]
                           x))
                clauses
        else return False
isFunctionSymbolRecursive _ _ = error "No function symbol provided."

isFunctionSymbolRecursive_
    :: Implicit_ [Clause]
    => String
    -> [Either String String]
    -> Clause
    -> Control.Monad.State.State Int Bool
isFunctionSymbolRecursive_ f hrs c = do
    his <-
        mapMaybeM
            (\(x,y) -> do
                 sub <- unify' x (fst y) --TODO: save state at beginning of this function and restore it at the end
                 if isJust sub
                     then return
                              (Just
                                   ((apply (fromJust sub) ***
                                     fmap (apply (fromJust sub)))
                                        y))
                     else return Nothing)
            (filter
                 (\(x,y) ->
                       Data.Rewriting.Term.root x ==
                       Data.Rewriting.Term.root (fst y))
                 (maybe [] getMetaPredications (snd c) `cartesianProduct`
                  param_))
    if null his
        then return False
        else if any
                    (\x ->
                          Data.Rewriting.Term.root (fst x) `elem` hrs)
                    his
                 then return True
                 else do
                     b <-
                         (fmap or .
                          mapM --anyM
                              (\x ->
                                    isFunctionSymbolRecursive_
                                        f
                                        (Data.Rewriting.Term.root (fst x) : hrs)
                                        x))
                             his
                     return (b && not (null his))

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
