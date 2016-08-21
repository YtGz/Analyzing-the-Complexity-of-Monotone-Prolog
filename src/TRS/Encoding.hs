module TRS.Encoding where

import Control.Monad.State
import SymbolicEvaluationGraphs.Types (AbstractState, G)
import SymbolicEvaluationGraphs.InferenceRules (nextG)
import ExprToTerm.Conversion (Term')
import Data.List (intersect)
import Data.Rewriting.Term (Term(..), vars)
import Data.Rewriting.Substitution (apply)
import Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty))

encodeIn :: BTree (AbstractState, (String, Int)) -> State Int Term'
encodeIn (BNode (([(ts,_,_)],(g,_)),("instance",_)) l@(BNode (([(_,mu,_)],_),("instanceChild",_)) Empty Empty) _) =
    fmap (apply mu) (encodeIn l)
encodeIn (BNode (([(ts,_,_)],(g,_)),_) _ _) = do
    i <- get
    modify (+ 1)
    return
        (Fun ("fin" ++ show i) (concatMap (map Var . vars) ts `intersect` g))
encodeIn _ = error "Cannot encode abstract state: multiple goals."

encodeOut :: BTree (AbstractState, (String, Int)) -> StateT Int IO Term'
encodeOut (BNode (([(ts,_,_)],(g,_)),("instance",_)) l@(BNode (([(_,mu,_)],_),("instanceChild",_)) Empty Empty) _) =
    fmap (apply mu) (encodeOut l)
encodeOut (BNode (([(ts,_,_)],(g,_)),_) _ _) = do
    i <- get
    modify (+ 1)
    gOut <- lift (nextGOnQuery ts g)
    return (Fun ("fout" ++ show i) gOut)
encodeOut _ = error "Cannot encode abstract state: multiple goals."

nextGOnQuery :: [Term'] -> G -> IO G
nextGOnQuery [] g = return g
nextGOnQuery (x:xs) g = nextGOnQuery xs =<< nextG x g

connectionPathStartNodes :: BTree (AbstractState, (String, Int))
                         -> [BTree (AbstractState, (String, Int))]
connectionPathStartNodes graph =
    let iCs = instanceChildren graph
    in filter
           (\x ->
                 case x of
                     BNode{} -> True
                     Empty -> False)
           (graph :
            fix
                (\f n ->
                      case n of
                          BNode x l r
                            | fst (snd x) == "instance" ->
                                let instanceChild = 
                                        head
                                            (filter
                                                 (\(BNode (_,(_,i)) _ _) ->
                                                       i ==
                                                       (\(BNode (_,(_,i)) _ _) ->
                                                             i)
                                                           l)
                                                 iCs)
                                in [ instanceChild
                                   | fst
                                        (snd
                                             ((\(BNode x _ _) ->
                                                    x)
                                                  instanceChild)) `notElem`
                                         ["instance", "split"] ] ++
                                   f l ++ f r
                            | fst (snd x) == "split" ->
                                [ l
                                | fst
                                     (snd
                                          ((\(BNode x _ _) ->
                                                 x)
                                               l)) `notElem`
                                      ["instance", "split"] ] ++
                                [ r
                                | fst
                                     (snd
                                          ((\(BNode x _ _) ->
                                                 x)
                                               r)) `notElem`
                                      ["instance", "split"] ] ++
                                f l ++ f r
                            | otherwise -> []
                          Empty -> [])
                graph)

connectionPathStartAndEndStates
    :: BTree (AbstractState, (String, Int))
    -> [((AbstractState, (String, Int)), (AbstractState, (String, Int)))]
connectionPathStartAndEndStates graph =
    let iCs = instanceChildren graph
    in concatMap
           (\x@(BNode y _ _) ->
                 map
                     (\x ->
                           (y, x))
                     (connectionPathEndStates iCs x))
           (connectionPathStartNodes graph)

connectionPathEndStates
    :: [BTree (AbstractState, (String, Int))]
    -> BTree (AbstractState, (String, Int))
    -> [(AbstractState, (String, Int))]
connectionPathEndStates iCs Empty = []
connectionPathEndStates iCs (BNode x l r)
  | any
       (\y ->
             fst (snd x) == y)
       ["instance", "split"] ||
        any
            (\(BNode y _ _) ->
                  snd (snd x) == snd (snd y) && fst (snd y) /= "instanceChild")
            iCs =
      [x]
  | fst (snd x) == "suc" =
      [x] ++ connectionPathEndStates iCs l ++ connectionPathEndStates iCs r
  | otherwise = connectionPathEndStates iCs l ++ connectionPathEndStates iCs r

instanceChildren :: BTree (AbstractState, (String, Int))
                 -> [BTree (AbstractState, (String, Int))]
instanceChildren graph =
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
             graph)
