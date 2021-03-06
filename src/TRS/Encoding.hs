{-# OPTIONS_GHC -Wall   #-}
module TRS.Encoding where

import Control.Monad.State
import Data.List (intersect, union, nub, nubBy, (\\))
import Data.Map
       (Map, difference, fromList, toList, intersectionWith,
        filterWithKey)
import qualified Data.Map (filter)
import Data.Maybe (fromJust, isJust, listToMaybe)

import Data.Rewriting.Rule (Rule(..))
import Data.Rewriting.Substitution (apply, merge)
import Data.Rewriting.Substitution.Type (toMap, fromMap)
import Data.Rewriting.Term (Term(..), vars)
import Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty))

import ExprToTerm.Conversion (Term', Subst', Rule')
import SymbolicEvaluationGraphs.InferenceRules (nextG)
import SymbolicEvaluationGraphs.Types (AbstractState, G)

generateRewriteRules
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [Rule']
generateRewriteRules graph =
    let iCs = instanceChildren graph graph
    in generateRewriteRules_ graph iCs

generateRewriteRules_
    :: BTree (AbstractState, (String, Int))
    -> [BTree (AbstractState, (String, Int))]
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [Rule']
generateRewriteRules_ graph iCs =
    liftM2
        (++)
        (liftM2 (++) (encodeConnectionPaths graph iCs) (encodeSplitRules graph))
        (encodeParallelRules graph)

generateRewriteRulesForGraphsWithMultSplitNodes
    :: BTree (AbstractState, (String, Int))
    -> [Int]
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [[Rule']]
generateRewriteRulesForGraphsWithMultSplitNodes graph mulSplitNodes = do
    assertDecomposability graph mulSplitNodes
    let subGraphs = generateSubGraphsForMultSplit graph mulSplitNodes
        iCs = instanceChildren graph graph
    mapM (`generateRewriteRules_` iCs) subGraphs

encodeIn :: BTree (AbstractState, (String, Int)) -> Term'
encodeIn (BNode (_,("instance",_)) l@(BNode (([(_,mu,_)],(_,_)),("instanceChild",_)) Empty Empty) _) =
    apply mu (encodeIn l)
encodeIn (BNode (_,("noChildInstance",_)) l@(BNode (([(_,mu,_)],(_,_)),("noChildInstanceChild",_)) Empty Empty) _) =
    apply mu (encodeIn l)
encodeIn (BNode ((ss,(g,_)),(_,i)) _ _) =
    Fun
        ("fin_s" ++ show i)
        (nub
             (concatMap
                  (concatMap (map Var . vars) .
                   (\(ts,_,_) ->
                         ts))
                  ss) `intersect`
         g)
encodeIn _ = error "Cannot encode abstract state due to malformed syntax"

encodeOut
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [Term']
encodeOut (BNode (_,("instance",_)) l@(BNode (([(_,mu,_)],_),("instanceChild",_)) Empty Empty) _) =
    fmap (map (apply mu)) (encodeOut l)
encodeOut (BNode (_,("noChildInstance",_)) l@(BNode (([(_,mu,_)],_),("noChildInstanceChild",_)) Empty Empty) _) =
    fmap (map (apply mu)) (encodeOut l)
encodeOut (BNode ((ss,(g,_)),(_,i)) _ _) =
    zipWithM
        f
        (map
             (\(ts,_,_) ->
                   ts)
             ss)
        [0 :: Integer ..]
  where
    f ts n = do
        nG <- nextGOnQuery ts g
        let gOut = nub nG \\ g
        return (Fun ("fout_s" ++ show i ++ "_" ++ show n) gOut)
encodeOut _ = error "Cannot encode abstract state: multiple goals."

nextGOnQuery
    :: [Term']
    -> G
    -> StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) G
nextGOnQuery [] g = return g
nextGOnQuery (x:xs) g =
    let nG = liftM2 union (return g) (nextG x g)
    in liftM2 union nG (nextGOnQuery xs =<< nG)

connectionPathStartNodes
    :: BTree (AbstractState, (String, Int))
    -> [BTree (AbstractState, (String, Int))]
    -> [BTree (AbstractState, (String, Int))]
connectionPathStartNodes graph iCs =
    nubBy
        (\(BNode (_,(_,i)) _ _) (BNode (_,(_,j)) _ _) ->
              i == j)
        (filter
             (\x ->
                   case x of
                       BNode{} -> True
                       Empty -> False)
             ([ graph
              | (\(BNode x _ _) ->
                      fst (snd x) /= "instance" &&
                      fst (snd x) /= "split" && fst (snd x) /= "parallel")
                   graph ] ++
              fix
                  (\f n visited ->
                        case n of
                            BNode x l r
                              | fst (snd x) == "instance" ->
                                  let instanceChild =
                                          head
                                              (filter
                                                   (\(BNode (_,(_,i)) _ _) ->
                                                         i ==
                                                         (\(BNode (_,(_,i')) _ _) ->
                                                               i')
                                                             l)
                                                   iCs)
                                  in [ instanceChild
                                     | fst
                                          (snd
                                               ((\(BNode x' _ _) ->
                                                      x')
                                                    instanceChild)) `notElem`
                                           ["instance", "split", "parallel"] ] ++
                                     if snd (snd x) /= -1 &&
                                        snd (snd x) `elem` visited
                                         then []
                                         else f
                                                  instanceChild
                                                  (snd (snd x) : visited)
                              | fst (snd x) == "split" ||
                                    fst (snd x) == "parallel" ->
                                  [ l
                                  | (\(BNode x' _ _) ->
                                          fst (snd x'))
                                       l `notElem`
                                        [ "instance"
                                        , "split"
                                        , "parallel"
                                        , "noChild"
                                        , "noChildInstance"] ] ++
                                  [ r
                                  | (\(BNode x' _ _) ->
                                          fst (snd x'))
                                       r `notElem`
                                        [ "instance"
                                        , "split"
                                        , "parallel"
                                        , "noChild"
                                        , "noChildInstance"] ] ++
                                  if snd (snd x) /= -1 &&
                                     snd (snd x) `elem` visited
                                      then []
                                      else f l (snd (snd x) : visited) ++
                                           f r (snd (snd x) : visited)
                              | otherwise ->
                                  if snd (snd x) /= -1 &&
                                     snd (snd x) `elem` visited
                                      then []
                                      else f l (snd (snd x) : visited) ++
                                           f r (snd (snd x) : visited)
                            Empty -> [])
                  graph
                  []))

connectionPathStartAndEndNodes
    :: BTree (AbstractState, (String, Int))
    -> [BTree (AbstractState, (String, Int))]
    -> [(BTree (AbstractState, (String, Int)), BTree (AbstractState, (String, Int)))]
connectionPathStartAndEndNodes graph iCs =
    concatMap
        (\x ->
              map
                  (\y ->
                        (x, y))
                  (connectionPathEndNodes x iCs))
        (connectionPathStartNodes graph iCs)

connectionPathEndNodes
    :: BTree (AbstractState, (String, Int))
    -> [BTree (AbstractState, (String, Int))]
    -> [BTree (AbstractState, (String, Int))]
connectionPathEndNodes Empty _ = error "Empty start node."
connectionPathEndNodes (BNode _ l r) iCs =
    connectionPathEndNodes_ l iCs ++ connectionPathEndNodes_ r iCs

connectionPathEndNodes_
    :: BTree (AbstractState, (String, Int))
    -> [BTree (AbstractState, (String, Int))]
    -> [BTree (AbstractState, (String, Int))]
connectionPathEndNodes_ Empty _ = []
connectionPathEndNodes_ n@(BNode x l r) iCs
  | any
       (\y ->
             fst (snd x) == y)
       ["instance", "split", "parallel"] ||
        any
            (\(BNode y _ _) ->
                  snd (snd x) == snd (snd y) && fst (snd y) /= "instanceChild")
            iCs =
      [n]
  | fst (snd x) == "suc" =
      [n] ++ connectionPathEndNodes_ l iCs ++ connectionPathEndNodes_ r iCs
  | otherwise = connectionPathEndNodes_ l iCs ++ connectionPathEndNodes_ r iCs

instanceChildren
    :: BTree (AbstractState, (String, Int))
    -> BTree (AbstractState, (String, Int))
    -> [BTree (AbstractState, (String, Int))]
instanceChildren graph startNode =
    fix
        (\f n is ->
              case n of
                  BNode (_,(x,i)) l r ->
                      [ n
                      | i `elem` is && x /= "instanceChild" ] ++
                      f l is ++ f r is
                  Empty -> [])
        graph
        (fix
             (\f n ->
                   case n of
                       BNode (_,(x,i)) l r ->
                           [ i
                           | x == "instanceChild" ] ++
                           f l ++ f r
                       Empty -> [])
             startNode)

-- theta `subDif` (sigma `compose` theta) == sigma
subDif
    :: Subst' -> Subst' -> Subst'
subDif t s =
    fromJust
        (merge
             (fromMap
                  (fromList
                       (map
                            (\(_,(x,y)) ->
                                  (x, y))
                            (toList
                                 (intersectionWith
                                      (\(Var x) y ->
                                            (x, y))
                                      (Data.Map.filter
                                           (\x ->
                                                 case x of
                                                     Var _ -> True
                                                     _ -> False)
                                           (toMap t))
                                      (toMap s))))))
             (fromMap (difference (toMap s) (toMap t))))

unifiersAppliedOnPath
    :: (BTree (AbstractState, (String, Int)), BTree (AbstractState, (String, Int)))
    -> Subst'
unifiersAppliedOnPath (BNode (((_,t,_):_,_),_) _ _,BNode (((_,s,_):_,_),_) _ _) =
    subDif t s
unifiersAppliedOnPath _ = error "Malformed connection path"

encodeConnectionPaths
    :: BTree (AbstractState, (String, Int))
    -> [BTree (AbstractState, (String, Int))]
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [Rule']
encodeConnectionPaths graph iCs =
    fmap
        concat
        (mapM encodeConnectionPath (connectionPathStartAndEndNodes graph iCs))

encodeConnectionPath
    :: (BTree (AbstractState, (String, Int)), BTree (AbstractState, (String, Int)))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [Rule']
encodeConnectionPath (s,e) = do
    gI <- generalizationInformationOnPath s e
    let sub = unifiersAppliedOnPath (s, e)
        applyGI t = foldr apply t gI
    if fst
           (snd
                ((\(BNode x _ _) ->
                       x)
                     e)) ==
       "suc"
        then do
            eO <- encodeOut s
            return
                (map
                     (Rule (applyGI (apply sub (encodeIn s))) .
                      applyGI . apply sub)
                     eO)
        else do
            eOs1 <- encodeOut s
            eOsk <- encodeOut e
            return
                (Rule
                     (applyGI (apply sub (encodeIn s)))
                     (applyGI
                          (Fun
                               (getUFunctionSymbol s e)
                               (encodeIn e :
                                nub
                                    ((\(Fun _ ts) ->
                                           ts)
                                         (apply sub (encodeIn s)))))) :
                 concatMap
                     (\x ->
                           map
                               (\y ->
                                     Rule
                                         (applyGI
                                              (Fun
                                                   (getUFunctionSymbol s e)
                                                   (y :
                                                    nub
                                                        ((\(Fun _ ts) ->
                                                               ts)
                                                             (apply
                                                                  sub
                                                                  (encodeIn s))))))
                                         (applyGI (apply sub x)))
                               eOsk)
                     eOs1)

encodeSplitRules
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [Rule']
encodeSplitRules graph =
    fmap
        concat
        (mapM
             encodeSplitRule
             (fix
                  (\f n ->
                        case n of
                            (BNode (_,(s,_)) l r) ->
                                [ n
                                | s == "split" ] ++
                                f l ++ f r
                            Empty -> [])
                  graph))

encodeSplitRule
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [Rule']
encodeSplitRule s@(BNode _ s1 s2@(BNode (((_,delta,_):_,_),(_,_)) _ _)) = do
    eOs <- encodeOut s
    eOs1 <- encodeOut s1
    eOs2 <- encodeOut s2
    return
        (Rule
             (encodeIn s)
             (Fun
                  (getUFunctionSymbol s s1)
                  (encodeIn s1 :
                   nub
                       ((\(Fun _ ts) ->
                              ts)
                            (encodeIn s)))) :
         map
             (\x ->
                   Rule
                       (Fun
                            (getUFunctionSymbol s s1)
                            (apply delta x :
                             nub
                                 ((\(Fun _ ts) ->
                                        ts)
                                      (encodeIn s))))
                       (Fun
                            (getUFunctionSymbol s1 s2)
                            (encodeIn s2 :
                             nub
                                 ((\(Fun _ ts) ->
                                        ts)
                                      (encodeIn s)) `union`
                             nub
                                 ((\(Fun _ ts) ->
                                        ts)
                                      (apply delta x)))))
             eOs1 ++
         concatMap
             (\x ->
                   concatMap
                       (\y ->
                             map
                                 (\z ->
                                       Rule
                                           (Fun
                                                (getUFunctionSymbol s1 s2)
                                                (z :
                                                 nub
                                                     ((\(Fun _ ts) ->
                                                            ts)
                                                          (encodeIn s)) `union`
                                                 nub
                                                     ((\(Fun _ ts) ->
                                                            ts)
                                                          (apply delta y))))
                                           (apply delta x))
                                 eOs2)
                       eOs1)
             eOs)
encodeSplitRule _ = error "Cannot encode split node due to malformed syntax"

encodeParallelRules
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [Rule']
encodeParallelRules graph =
    fmap
        concat
        (mapM
             encodeParallelRule
             (fix
                  (\f n ->
                        case n of
                            (BNode (_,(s,_)) l r) ->
                                [ n
                                | s == "parallel" ] ++
                                f l ++ f r
                            Empty -> [])
                  graph))

encodeParallelRule
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) [Rule']
encodeParallelRule s@(BNode _ s1 s2) = do
    eOs <- encodeOut s
    [eOs1] <- encodeOut s1
    eOs2 <- encodeOut s2
    return
        (Rule
             (encodeIn s)
             (Fun
                  (getUFunctionSymbol s s1 ++ "_" ++ getUFunctionSymbol s s2)
                  (encodeIn s1 :
                   encodeIn s2 :
                   nub
                       ((\(Fun _ ts) ->
                              ts)
                            (encodeIn s)))) :
         Rule
             (Fun
                  (getUFunctionSymbol s s1 ++ "_" ++ getUFunctionSymbol s s2)
                  (eOs1 :
                   Var "P" :
                   nub
                       ((\(Fun _ ts) ->
                              ts)
                            (encodeIn s))))
             (head eOs) :
         zipWith
             (\x y ->
                   Rule
                       (Fun
                            (getUFunctionSymbol s s1 ++
                             "_" ++ getUFunctionSymbol s s2)
                            (Var "P" :
                             x :
                             nub
                                 ((\(Fun _ ts) ->
                                        ts)
                                      (encodeIn s))))
                       y)
             eOs2
             (tail eOs))
encodeParallelRule _ =
    error "Cannot encode parallel node due to malformed syntax"

getUFunctionSymbol
    :: BTree (AbstractState, (String, Int))
    -> BTree (AbstractState, (String, Int))
    -> String
getUFunctionSymbol x y =
    "u_s" ++
    show
        (snd
             (snd
                  ((\(BNode x' _ _) ->
                         x')
                       x))) ++
    "_s" ++
    show
        (snd
             (snd
                  ((\(BNode x' _ _) ->
                         x')
                       y)))

assertDecomposability
    :: Applicative f
    => BTree (AbstractState, (String, Int)) -> [Int] -> f ()
assertDecomposability graph mulSplitNodes =
    when
        (any
             (\i ->
                   any
                       (\x ->
                             isJust (getNode x i))
                       (instanceChildren graph (fromJust (getNode graph i))))
             mulSplitNodes)
        (error "The graph is not decomposable. Aborting analysis.")

getNode
    :: BTree (AbstractState, (String, Int))
    -> Int
    -> Maybe (BTree (AbstractState, (String, Int)))
getNode graph i =
    listToMaybe
        (fix
             (\f n ->
                   case n of
                       BNode (_,(_,j)) l r ->
                           if i == j
                               then [n]
                               else f l ++ f r
                       Empty -> [])
             graph)

generateSubGraphsForMultSplit
    :: BTree (AbstractState, (String, Int))
    -> [Int]
    -> [BTree (AbstractState, (String, Int))]
generateSubGraphsForMultSplit graph is =
    map
        (`endGraphAtMultSplitNodes` is)
        (graph : getMultSplitNodeChildren graph is)

getMultSplitNodeChildren
    :: BTree (AbstractState, (String, Int))
    -> [Int]
    -> [BTree (AbstractState, (String, Int))]
getMultSplitNodeChildren graph is =
    concatMap
        (\(BNode _ l r) ->
              (case l of
                   Empty -> []
                   BNode{} -> [l]) ++
              (case r of
                   Empty -> []
                   BNode{} -> [r]))
        (fix
             (\f n ->
                   case n of
                       (BNode (_,(s,i)) l r) ->
                           [ n
                           | i `elem` is && s /= "instanceChild" ] ++
                           f l ++ f r
                       Empty -> [])
             graph)

endGraphAtMultSplitNodes
    :: BTree (AbstractState, (String, Int))
    -> [Int]
    -> BTree (AbstractState, (String, Int))
endGraphAtMultSplitNodes Empty _ = Empty
endGraphAtMultSplitNodes (BNode x l r) is =
    if snd (snd x) `elem` is && fst (snd x) /= "instanceChild"
        then BNode x (f l) (f r)
        else BNode
                 x
                 (endGraphAtMultSplitNodes l is)
                 (endGraphAtMultSplitNodes r is)
  where
    f (BNode (as,("instance",i)) (BNode (as',("instanceChild",i')) _ _) _) =
        BNode
            (as, ("noChildInstance", i))
            (BNode (as', ("noChildInstanceChild", i')) Empty Empty)
            Empty
    f (BNode (as,(_,i)) _ _) = BNode (as, ("noChild", i)) Empty Empty
    f _ = error "Malformed abstract state"

generalizationInformationOnPath
    :: BTree (AbstractState, (String, Int))
    -> BTree (AbstractState, (String, Int))
    -> StateT (Map (String, Int, [Int]) [Int]) (StateT (Map Int Subst') IO) (Map Int Subst')
generalizationInformationOnPath startNode endNode = do
    generalizationSteps <- lift get
    return
        (filterWithKey
             (curry
                  ((`elem` init (getNodeLabelsOnPath startNode endNode)) . fst))
             generalizationSteps) -- note that 'init' isn't strictly necessary, as the end node of a connection path can't be a generalization node anyway

getNodeLabelsOnPath
    :: BTree (AbstractState, (String, Int))
    -> BTree (AbstractState, (String, Int))
    -> [Int]
getNodeLabelsOnPath startNode (BNode (_,(_,endNode)) _ _) =
    snd (getNodeLabelsOnPath_ startNode endNode [])
getNodeLabelsOnPath _ Empty =
    error "Malformed connection path: end node can't be empty"

getNodeLabelsOnPath_ :: BTree (AbstractState, (String, Int))
                     -> Int
                     -> [Int]
                     -> (Bool, [Int])
getNodeLabelsOnPath_ Empty _ _ = (False, [])
getNodeLabelsOnPath_ (BNode n l r) endNode is =
    let is' = is ++ [snd (snd n)]
    in if snd (snd n) == endNode
           then (True, is')
           else let (x,y) =
                        ( getNodeLabelsOnPath_ l endNode is'
                        , getNodeLabelsOnPath_ r endNode is')
                in if fst x
                       then x
                       else y
