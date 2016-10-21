{-# OPTIONS_GHC -Wall    #-}
module SymbolicEvaluationGraphs.Heuristic where

import Control.Arrow ((***), second)
import Control.Lens (element, (.~))
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.State
import Control.Monad.Supply
import Data.Either.Utils
import Data.Foldable (toList)
import Data.List (find, maximumBy)
import Data.Map (Map, fromList, insertWith, union, elemAt)
import qualified Data.Map
       (insert, toList, empty, map, lookup, singleton)
import Data.Maybe
import Data.Ord (comparing)
import Data.Tree
import Data.Tree.Zipper

import Data.Rewriting.Pos
import Data.Rewriting.Substitution (unify, apply)
import Data.Rewriting.Substitution.Type (fromMap, toMap)
import Data.Rewriting.Term (vars, subtermAt, replaceAt)
import qualified Data.Rewriting.Term (root)
import Data.Rewriting.Term.Type (Term(..))
import Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty))

import ExprToTerm.Conversion
import SymbolicEvaluationGraphs.InferenceRules
       (suc, caseRule, eval, backtrack, isBacktrackingApplicable, split,
        tryToApplyInstanceRule, parallel, arityOfRootSymbol,
        implicitGeneralization)
import SymbolicEvaluationGraphs.Types
import SymbolicEvaluationGraphs.Utilities
       (freshAbstractVariable, instantiateWithFreshAbstractVariables)
import Query.Utilities

minExSteps :: Int
minExSteps = 1

maxBranchingFactor :: Int
maxBranchingFactor = 4

finiteGeneralizationDepth :: Int
finiteGeneralizationDepth = 8

finiteGeneralizationPos :: Int
finiteGeneralizationPos = 2

graphSizeLimit :: Int
graphSizeLimit = 400

generateSymbolicEvaluationGraph
    :: [Clause]
    -> QueryClass
    -> StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) (BTree (AbstractState, (String, Int)))
generateSymbolicEvaluationGraph clauses queryClass = do
    tp <-
        evalSupplyT
            (evalStateT
                 (do initialState <- getInitialAbstractState queryClass
                     applyRule
                         clauses
                         (return (fromTree (Node (initialState, ("", -1)) [])))
                         0)
                 (0,0))
            [0 ..]
    return (roseTreeToBTree (toTree tp))

type TreePath a = [TreePos Full a -> TreePos Full a]

firstChild' :: TreePos Full a -> TreePos Full a
firstChild' = fromJust . firstChild

parent' :: TreePos Full a -> TreePos Full a
parent' = fromJust . parent

prev' :: TreePos Full a -> TreePos Full a
prev' = fromJust . prev

next' :: TreePos Full a -> TreePos Full a
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
    => QueryClass -> StateT (Int,Int) m AbstractState
getInitialAbstractState (f,m) = do
    (vs,gs) <-
        foldr
            (\x s -> do
                 (vs,gs) <- s
                 v <- freshAbstractVariable
                 case x of
                     In -> return (v : vs, v : gs)
                     Out -> return (v : vs, gs))
            (return ([], []))
            m
    return ([([Fun f vs], fromMap (fromList []), Nothing)], (gs, []))

applyRule
    :: [Clause]
    -> IO (TreePos Full (AbstractState, (String, Int)))
    -> Int
    -> StateT (Int,Int) (SupplyT Int (StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) (TreePos Full (AbstractState, (String, Int)))
applyRule clauses ioTp n = do
    treePos <- Control.Monad.State.liftIO ioTp
    progress <- peek
    if progress > graphSizeLimit
        then return treePos
        else do
            let s = fst (label treePos)
            let ss =
                    eval s :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) (AbstractState, AbstractState)
                s0 = do
                    ev <- ss
                    return (fst ev)
                s1 = do
                    ev <- ss
                    return (snd ev)
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
                        ((_:_,_,_):_,_) -> do
                            j <-
                                if snd (snd (label treePos)) == -1
                                    then peek :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                                    else return (snd (snd (label treePos)))
                            let newTp =
                                    modifyLabel
                                        (\(x,_) ->
                                              (x, ("instance", j)))
                                        treePos
                            instanceCandidates <-
                                getInstanceCandidates
                                    clauses
                                    (label treePos)
                                    (roseTreeToBTree (toTree newTp))
                            let inst =
                                    tryToApplyInstanceRule s instanceCandidates
                            if isJust inst
                                then do
                                    j' <-
                                        if snd (snd (label treePos)) == -1
                                            then supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                                            else return
                                                     (snd (snd (label treePos)))
                                    let newTp' =
                                            modifyLabel
                                                (\(x,_) ->
                                                      (x, ("instance", j')))
                                                treePos
                                    return
                                        (Just
                                             (fst
                                                  (insertAndMoveToChild
                                                       newTp'
                                                       (inst, Nothing))))
                                else return Nothing
                        _ -> return Nothing
                b0 x' y = do
                    x <- x'
                    nT <- applyRule clauses (return (fst y)) n
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
                    nT <- applyRule clauses (return y) n
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
                    j1 <-
                        supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                    j2 <-
                        supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                    return
                        (insertAndMoveToChild
                             treePos
                             (Just (s0', ("", j1)), Just (s1', ("", j2))))
                cs1 = do
                    sp0' <- sp0
                    sp1' <- sp1
                    j1 <-
                        supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                    j2 <-
                        supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                    return
                        (insertAndMoveToChild
                             treePos
                             (Just (sp0', ("", j1)), Just (sp1', ("", j2))))
                cs2 = do
                    j1 <-
                        supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                    j2 <-
                        supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                    return
                        (insertAndMoveToChild
                             treePos
                             (Just (par0, ("", j1)), Just (par1, ("", j2))))
                e = do
                    j <-
                        if snd (snd (label treePos)) == -1
                            then supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                            else return (snd (snd (label treePos)))
                    cs0' <- cs0
                    l <-
                        b0
                            (return
                                 (modifyLabel
                                      (\(x,_) ->
                                            (x, ("eval", j)))
                                      treePos))
                            cs0'
                    let tp =
                            insert
                                (tree (snd cs0'))
                                (Data.Tree.Zipper.last (children l))
                    b1 l tp
                sp = do
                    j <-
                        if snd (snd (label treePos)) == -1
                            then supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                            else return (snd (snd (label treePos)))
                    cs1' <- cs1
                    l <-
                        b0
                            (return
                                 (modifyLabel
                                      (\(x,_) ->
                                            (x, ("split", j)))
                                      treePos))
                            cs1'
                    let tp =
                            insert
                                (tree (snd cs1'))
                                (Data.Tree.Zipper.last (children l))
                    b1 l tp
                par = do
                    j <-
                        if snd (snd (label treePos)) == -1
                            then supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                            else return (snd (snd (label treePos)))
                    cs2' <- cs2
                    l <-
                        b0
                            (return
                                 (modifyLabel
                                      (\(x,_) ->
                                            (x, ("parallel", j)))
                                      treePos))
                            cs2'
                    let tp =
                            insert
                                (tree (snd cs2'))
                                (Data.Tree.Zipper.last (children l))
                    b1 l tp
                c = do
                    j <-
                        if snd (snd (label treePos)) == -1
                            then supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                            else return (snd (snd (label treePos)))
                    applyRule
                        clauses
                        (return
                             (fst
                                  (insertAndMoveToChild
                                       (modifyLabel
                                            (\(x,_) ->
                                                  (x, ("case", j)))
                                            treePos)
                                       ( Just (caseRule clauses s, ("", -1))
                                       , Nothing))))
                        (n + 1)
                gS = do
                    j <-
                        if snd (snd (label treePos)) == -1
                            then supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                            else return (snd (snd (label treePos)))
                    genStep <- applyGeneralizationStep s j
                    applyRule
                        clauses
                        (return
                             (fst
                                  (insertAndMoveToChild
                                       (modifyLabel
                                            (\(x,_) ->
                                                  (x, ("generalization", j)))
                                            treePos)
                                       ( Just
                                             ( implicitGeneralization genStep
                                             , ("", -1))
                                       , Nothing))))
                        (n + 1)
            case s of
                ([],_) ->
                    return
                        (fst
                             (insertAndMoveToChild
                                  (modifyLabel
                                       (\(x,_) ->
                                             (x, ("", -1)))
                                       treePos)
                                  (Nothing, Nothing)))
                (([],_,_):_,_) -> do
                    j <-
                        if snd (snd (label treePos)) == -1
                            then supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                            else return (snd (snd (label treePos)))
                    applyRule
                        clauses
                        (return
                             (fst
                                  (insertAndMoveToChild
                                       (modifyLabel
                                            (\(x,_) ->
                                                  (x, ("suc", j)))
                                            treePos)
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
                                if snd (snd (label treePos)) == -1
                                    then supply :: StateT (Int,Int) (SupplyT Int (StateT (Data.Map.Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) Int
                                    else return (snd (snd (label treePos)))
                            applyRule
                                clauses
                                (return
                                     (fst
                                          (insertAndMoveToChild
                                               (modifyLabel
                                                    (\(x,_) ->
                                                          (x, ("backtrack", j)))
                                                    treePos)
                                               ( Just (backtrack s, ("", -1))
                                               , Nothing))))
                                n
                        else do
                            b <-
                                case s of
                                    ((t:_,_,Nothing):_,_) ->
                                        if case Data.Rewriting.Term.root t of
                                               (Left _) -> True
                                               _ -> False
                                            then return False
                                            else isFunctionSymbolRecursive
                                                     clauses
                                                     (Fun
                                                          (fromRight
                                                               (Data.Rewriting.Term.root
                                                                    t))
                                                          [])
                                                     (arityOfRootSymbol t)
                                    ((_:_,_,Just clause):_,_) ->
                                        isClauseRecursive clauses clause
                                    _ -> error "Error in heuristic"
                            if n >= minExSteps && b
                                then if length (fst s) > 1
                                         then par
                                         else do
                                             i' <- i
                                             maybe
                                                 (if any
                                                         (isJust .
                                                          findFiniteGeneralizationPosition)
                                                         (concatMap
                                                              (\(x,_,_) ->
                                                                    x)
                                                              (fst s))
                                                      then gS
                                                      else (case s of
                                                                (([_],_,Nothing):_,_) ->
                                                                    c
                                                                (([_],_,_):_,_) ->
                                                                    e
                                                                _ -> sp))
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
    :: (Monad m)
    => [Clause]
    -> (AbstractState, (String, Int))
    -> BTree (AbstractState, (String, Int))
    -> StateT (Int,Int) m [(AbstractState, (String, Int))]
getInstanceCandidates clauses node graph = do
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
                     clauses
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
                   fst (snd x) /= "instanceChild" &&
                   (getVarNum (fst node) >= getVarNum (fst x) ||
                    (isRecursive &&
                     branchingFactor clauses (nodeHead node) >
                     maxBranchingFactor)))
             (Data.Foldable.toList graph))

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

isClauseRecursive
    :: (Monad m)
    => [Clause] -> Clause -> StateT (Int,Int) m Bool
isClauseRecursive _ (_,Nothing) = return False
isClauseRecursive clauses (_,Just b) =
    (fmap or .
     mapM --anyM
         (\x ->
               if case Data.Rewriting.Term.root x of
                      (Left _) -> True
                      _ -> False
                   then return False
                   else isFunctionSymbolRecursive
                            clauses
                            (Fun (fromRight (Data.Rewriting.Term.root x)) [])
                            (arityOfRootSymbol x)))
        (getMetaPredications b)

isFunctionSymbolRecursive
    :: Monad m
    => [Clause] -> Term' -> Int -> StateT (Int,Int) m Bool
isFunctionSymbolRecursive clauses (Fun f _) arity =
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
            clauses' <-
                mapMaybeM
                    (\x -> do
                         let startF =
                                 Fun
                                     f
                                     (map
                                          (Var . show)
                                          (take arity [1 :: Integer,2 ..]))
                         (x',_) <-
                             instantiateWithFreshAbstractVariables
                                 (fst x : maybeToList (snd x))
                         let sub = unify startF (head x') --TODO: save state at beginning of this function and restore it at the end
                         if isJust sub
                             then return
                                      (Just
                                           ((apply (fromJust sub) ***
                                             fmap (apply (fromJust sub)))
                                                {-(head x', listToMaybe (tail x'))-}x))
                             else return Nothing)
                    (filter
                         (\x ->
                               Right f == Data.Rewriting.Term.root (fst x))
                         clauses)
            (fmap or .
             mapM --anyM
                 (\x ->
                       isFunctionSymbolRecursive_
                           clauses
                           f
                           [Data.Rewriting.Term.root (fst x)]
                           x))
                clauses'
        else return False
isFunctionSymbolRecursive _ _ _ = error "No function symbol provided."

isFunctionSymbolRecursive_
    :: Monad m
    => [Clause]
    -> String
    -> [Either String String]
    -> Clause
    -> StateT (Int,Int) m Bool
isFunctionSymbolRecursive_ clauses f hrs c = do
    his <-
        mapMaybeM
            (\(x,y) -> do
                 (y',_) <-
                     instantiateWithFreshAbstractVariables
                         (fst y : maybeToList (snd y))
                 let sub = unify x (head y') --TODO: save state at beginning of this function and restore it at the end
                 if isJust sub
                     then return
                              (Just
                                   ((apply (fromJust sub) ***
                                     fmap (apply (fromJust sub)))
                                        {-(head y', listToMaybe (tail y'))-}y))
                     else return Nothing)
            (filter
                 (\(x,y) ->
                       Data.Rewriting.Term.root x ==
                       Data.Rewriting.Term.root (fst y))
                 (maybe [] getMetaPredications (snd c) `cartesianProduct`
                  clauses))
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
                                        clauses
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

branchingFactor :: [Clause] -> Term' -> Int
branchingFactor clauses (Fun f _) =
    length (filter (== Right f) (map (Data.Rewriting.Term.root . fst) clauses))
branchingFactor _ _ = error "No function symbol provided."

applyGeneralizationStep
    :: AbstractState
    -> Int
    -> StateT (Int,Int) (SupplyT Int (StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) AbstractState
applyGeneralizationStep s nodePos =
    applyGeneralizationStep_ [] s nodePos (fst (snd s))

applyGeneralizationStep_
    :: [(Term', Term')]
    -> AbstractState
    -> Int
    -> G
    -> StateT (Int,Int) (SupplyT Int (StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO))) AbstractState
applyGeneralizationStep_ r s nodePos g = do
    let ts =
            map
                (\x ->
                      (x, "s"))
                (concat
                     (zipWith
                          (\x y ->
                                map
                                    (\z ->
                                          (z, y))
                                    x)
                          (map
                               (\(x,_,_) ->
                                     zip x [0 ..])
                               (fst s))
                          [0 ..])) ++
            map
                (\x ->
                      (x, "kb"))
                (zip
                     (concatMap
                          (\(lhs,rhs) ->
                                [(lhs, 0), (rhs, 1)])
                          (snd (snd s)))
                     (concatMap (replicate 2) [0 ..]))
        tAnnotated =
            Data.List.find
                (isJust . findFiniteGeneralizationPosition . fst . fst . fst)
                ts
    if isJust tAnnotated
        then do
            let (((t,j),k),l) = fromJust tAnnotated
                pos = fromJust (findFiniteGeneralizationPosition t)
                fromR =
                    find
                        (\x ->
                              fromJust (subtermAt t pos) == fst x)
                        r
            freshVar <- freshAbstractVariable
            when
                (all
                     (`elem` fst (snd s))
                     (map Var (vars (fromJust (subtermAt t pos)))))
                (lift
                     (lift
                          (lift
                               (modify
                                    (insertWith
                                         (curry
                                              (fromMap .
                                               Data.Map.insert
                                                   ((\(Var v) ->
                                                          v)
                                                        freshVar)
                                                   (fromJust (subtermAt t pos)) .
                                               toMap . snd))
                                         nodePos
                                         (fromMap
                                              (Data.Map.singleton
                                                   ((\(Var v) ->
                                                          v)
                                                        freshVar)
                                                   (fromJust (subtermAt t pos)))))))))
            let (t',r',g')
                  | isJust fromR =
                      (fromJust (replaceAt t pos (snd (fromJust fromR))), r, g)
                  | all
                       (`elem` fst (snd s))
                       (map Var (vars (fromJust (subtermAt t pos)))) =
                      ( fromJust (replaceAt t pos freshVar)
                      , (fromJust (subtermAt t pos), freshVar) : r
                      , g ++ [freshVar])
                  | otherwise =
                      ( fromJust (replaceAt t pos freshVar)
                      , (fromJust (subtermAt t pos), freshVar) : r
                      , g)
                s'
                  | l == "s" =
                      (\(ss,kb) ->
                            ( (element k .~
                               (\(x,y,z) ->
                                     ((element j .~ t') x, y, z))
                                   (ss !! k))
                                  ss
                            , kb))
                          s
                  | l == "kb" && j == 0 =
                      (\(ss,(g'',u)) ->
                            (ss, (g'', (element k .~ (t', snd (u !! k))) u)))
                          s
                  | l == "kb" && j == 1 =
                      (\(ss,(g'',u)) ->
                            (ss, (g'', (element k .~ (fst (u !! k), t')) u)))
                          s
                  | otherwise =
                      error "Error in function applyGeneralizationStep_"
            applyGeneralizationStep_ r' s' nodePos g'
        else return (fst s, (g, snd (snd s)))

findFiniteGeneralizationPosition :: Term' -> Maybe Pos
findFiniteGeneralizationPosition t =
    let candidate =
            fromList
                [ second
                      (\x ->
                            fromList [x])
                      (maximumBy
                           (comparing (snd . snd))
                           (Data.Map.toList
                                (Data.Map.map
                                     (maximumBy (comparing snd) .
                                      Data.Map.toList)
                                     (countFunctionSymbolOccurences
                                          t
                                          Data.Map.empty
                                          []))))]
    in if snd (elemAt 0 (snd (elemAt 0 candidate))) >=
          finiteGeneralizationDepth
           then let ePos = fst (elemAt 0 candidate)
                in Just
                       (fix
                            (\f s t' pos i path ->
                                  case t' of
                                      Fun s' _
                                        | s' == s ->
                                            if i == finiteGeneralizationPos
                                                then pos
                                                else f
                                                         s
                                                         (fromJust
                                                              (subtermAt
                                                                   t'
                                                                   [head path]))
                                                         (pos ++ [head path])
                                                         (i + 1)
                                                         (tail path)
                                      _ ->
                                          f
                                              s
                                              (fromJust
                                                   (subtermAt t' [head path]))
                                              (pos ++ [head path])
                                              i
                                              (tail path))
                            (fst (elemAt 0 (snd (elemAt 0 candidate))))
                            t
                            []
                            1
                            ePos)
           else Nothing

countFunctionSymbolOccurences :: Term'
                              -> Map Pos (Map String Int)
                              -> Pos
                              -> Map Pos (Map String Int)
countFunctionSymbolOccurences (Fun f args) _ [] =
    let m' =
            Data.Map.insert
                []
                (Data.Map.insert f 1 Data.Map.empty)
                Data.Map.empty
    in foldl
           union
           m'
           (zipWith
                (\x y ->
                      countFunctionSymbolOccurences x m' [y])
                args
                [0 ..])
countFunctionSymbolOccurences (Fun f args) m pos =
    let m' =
            Data.Map.insert
                pos
                (insertWith (+) f 1 (fromJust (Data.Map.lookup (init pos) m)))
                m
    in foldl
           union
           m'
           (zipWith
                (\x y ->
                      countFunctionSymbolOccurences x m' (pos ++ [y]))
                args
                [0 ..])
countFunctionSymbolOccurences _ m _ = m

bTreeToRoseTree :: BTree a -> Tree a
bTreeToRoseTree (BNode e Empty Empty) = Node e []
bTreeToRoseTree (BNode e l Empty) = Node e [bTreeToRoseTree l]
bTreeToRoseTree (BNode e l r) = Node e [bTreeToRoseTree l, bTreeToRoseTree r]
bTreeToRoseTree _ = error "Malformed tree node"

roseTreeToBTree :: Tree a -> BTree a
roseTreeToBTree (Node e []) = BNode e Empty Empty
roseTreeToBTree (Node e [l]) = BNode e (roseTreeToBTree l) Empty
roseTreeToBTree (Node e [l,r]) =
    BNode e (roseTreeToBTree l) (roseTreeToBTree r)
roseTreeToBTree _ = error "Malformed tree node"
