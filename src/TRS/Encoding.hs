module TRS.Encoding where

import Control.Monad.State
import SymbolicEvaluationGraphs.Types (AbstractState, G)
import SymbolicEvaluationGraphs.InferenceRules (nextG)
import ExprToTerm.Conversion (Term', Subst', Rule')
import Data.List (intersect, union, nub, nubBy, (\\))
import Data.Map
       (Map, difference, fromList, toList, intersectionWith)
import qualified Data.Map (filter)
import Data.Maybe (fromJust)
import Control.Monad.Morph
import Data.Rewriting.Term (Term(..), vars)
import Data.Rewriting.Substitution (apply, merge)
import Data.Rewriting.Substitution.Type (toMap, fromMap)
import Data.Rewriting.Rule (Rule(..))
import Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty))

generateRewriteRules
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) IO [Rule']
generateRewriteRules graph =
    liftM2 (++) (encodeConnectionPaths graph) (encodeSplitRules graph)

generateRewriteRulesForGraphsWithMultSplitNodes
    :: BTree (AbstractState, (String, Int))
    -> [Int]
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) IO [[Rule']]
generateRewriteRulesForGraphsWithMultSplitNodes graph mulSplitNodes = do
    let subGraphs = generateSubGraphsForMultSplit graph mulSplitNodes
    mapM generateRewriteRules subGraphs

encodeIn :: BTree (AbstractState, (String, Int)) -> Term'
encodeIn (BNode (([(_,_,_)],_),("instance",_)) l@(BNode (([(ts,mu,_)],(g,_)),("instanceChild",i)) Empty Empty) _) =
    Fun
        ("fin_s" ++ show i)
        (nub (concatMap ((map Var . vars) . apply mu) ts) `intersect`
         map (apply mu) g)
encodeIn (BNode (([(ts,_,_)],(g,_)),(_,i)) _ _) =
    Fun ("fin_s" ++ show i) (nub (concatMap (map Var . vars) ts) `intersect` g)
encodeIn _ = error "Cannot encode abstract state: multiple goals."

encodeOut
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) IO Term'
encodeOut (BNode (([(ts,_,_)],(g,_)),("instance",_)) l@(BNode (([(_,mu,_)],_),("instanceChild",_)) Empty Empty) _) =
    fmap (apply mu) (encodeOut l)
encodeOut (BNode (([(ts,_,_)],(g,_)),(_,i)) _ _) = do
    nG <- nextGOnQuery ts g
    let gOut = nub nG \\ g
    return (Fun ("fout_s" ++ show i) gOut)
encodeOut _ = error "Cannot encode abstract state: multiple goals."

nextGOnQuery :: [Term']
             -> G
             -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) IO G
nextGOnQuery [] g = return g
nextGOnQuery (x:xs) g = nextGOnQuery xs =<< nextG x g

connectionPathStartNodes :: BTree (AbstractState, (String, Int))
                         -> [BTree (AbstractState, (String, Int))]
connectionPathStartNodes graph =
    let iCs = instanceChildren graph
    in nubBy
           (\(BNode (_,(_,i)) _ _) (BNode (_,(_,j)) _ _) ->
                 i == j)
           (filter
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
                                             (\x ->
                                                   case x of
                                                       [] ->
                                                           error
                                                               "The graph is not decomposable. Analysis not possible."
                                                       _ -> head x)
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
                                     | (\(BNode x _ _) ->
                                             fst (snd x))
                                          l `notElem`
                                           ["instance", "split", "noChild"] ] ++
                                     [ r
                                     | (\(BNode x _ _) ->
                                             fst (snd x))
                                          r `notElem`
                                           ["instance", "split", "noChild"] ] ++
                                     f l ++ f r
                                 | otherwise -> f l ++ f r
                               Empty -> [])
                     graph))

connectionPathStartAndEndNodes
    :: BTree (AbstractState, (String, Int))
    -> [(BTree (AbstractState, (String, Int)), BTree (AbstractState, (String, Int)))]
connectionPathStartAndEndNodes graph =
    let iCs = instanceChildren graph
    in concatMap
           (\x ->
                 map
                     (\y ->
                           (x, y))
                     (connectionPathEndNodes iCs x))
           (connectionPathStartNodes graph)

connectionPathEndNodes
    :: [BTree (AbstractState, (String, Int))]
    -> BTree (AbstractState, (String, Int))
    -> [BTree (AbstractState, (String, Int))]
connectionPathEndNodes _ Empty = error "Empty start node."
connectionPathEndNodes iCs (BNode _ l r) =
    connectionPathEndNodes_ iCs l ++ connectionPathEndNodes_ iCs r

connectionPathEndNodes_
    :: [BTree (AbstractState, (String, Int))]
    -> BTree (AbstractState, (String, Int))
    -> [BTree (AbstractState, (String, Int))]
connectionPathEndNodes_ iCs Empty = []
connectionPathEndNodes_ iCs n@(BNode x l r)
  | any
       (\y ->
             fst (snd x) == y)
       ["instance", "split"] ||
        any
            (\(BNode y _ _) ->
                  snd (snd x) == snd (snd y) && fst (snd y) /= "instanceChild")
            iCs =
      [n]
  | fst (snd x) == "suc" =
      [n] ++ connectionPathEndNodes_ iCs l ++ connectionPathEndNodes_ iCs r
  | otherwise = connectionPathEndNodes_ iCs l ++ connectionPathEndNodes_ iCs r

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
unifiersAppliedOnPath (BNode (([(_,t,_)],_),_) _ _,BNode (((_,s,_):_,_),_) _ _) =
    subDif t s

encodeConnectionPaths
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) IO [Rule']
encodeConnectionPaths graph =
    fmap
        concat
        (mapM encodeConnectionPath (connectionPathStartAndEndNodes graph))

encodeConnectionPath
    :: (BTree (AbstractState, (String, Int)), BTree (AbstractState, (String, Int)))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) IO [Rule']
encodeConnectionPath (s,e) =
    if fst
           (snd
                ((\(BNode x _ _) ->
                       x)
                     e)) ==
       "suc"
        then do
            eO <- encodeOut s
            return [Rule (apply sub (encodeIn s)) (apply sub eO)]
        else do
            eOs1 <- encodeOut s
            eOsk <- encodeOut e
            return
                [ Rule
                      (apply sub (encodeIn s))
                      (Fun
                           (getUFunctionSymbol s e)
                           (encodeIn e :
                            map Var (vars (apply sub (encodeIn s)))))
                , Rule
                      (Fun
                           (getUFunctionSymbol s e)
                           (eOsk : map Var (vars (apply sub (encodeIn s)))))
                      (apply sub eOs1)]
  where
    sub = unifiersAppliedOnPath (s, e)

encodeSplitRules
    :: BTree (AbstractState, (String, Int))
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) IO [Rule']
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
    -> Control.Monad.State.StateT (Map (String, Int, [Int]) [Int]) IO [Rule']
encodeSplitRule s@(BNode _ s1@(BNode (((_,delta,_):_,_),_) _ _) s2) = do
    eOs <- encodeOut s
    eOs1 <- encodeOut s1
    eOs2 <- encodeOut s2
    return
        [ Rule
              (encodeIn s)
              (Fun
                   (getUFunctionSymbol s s1)
                   (encodeIn s1 : map Var (vars (encodeIn s))))
        , Rule
              (Fun
                   (getUFunctionSymbol s s1)
                   (apply delta eOs1 : map Var (vars (encodeIn s))))
              (Fun
                   (getUFunctionSymbol s1 s2)
                   (encodeIn s2 :
                    map Var (vars (encodeIn s)) `union`
                    map Var (vars (apply delta eOs1))))
        , Rule
              (Fun
                   (getUFunctionSymbol s1 s2)
                   (eOs2 :
                    map Var (vars (encodeIn s)) `union`
                    map Var (vars (apply delta eOs1))))
              (apply delta eOs)]

getUFunctionSymbol
    :: BTree (AbstractState, (String, Int))
    -> BTree (AbstractState, (String, Int))
    -> String
getUFunctionSymbol x y =
    "u_s" ++
    show
        (snd
             (snd
                  ((\(BNode x _ _) ->
                         x)
                       x))) ++
    "_s" ++
    show
        (snd
             (snd
                  ((\(BNode x _ _) ->
                         x)
                       y)))

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
        then BNode
                 x
                 ((\(BNode (as,(r,i)) _ _) ->
                        BNode (as, ("noChild", i)) Empty Empty)
                      l)
                 ((\(BNode (as,(r,i)) _ _) ->
                        BNode (as, ("noChild", i)) Empty Empty)
                      r)
        else BNode
                 x
                 (endGraphAtMultSplitNodes l is)
                 (endGraphAtMultSplitNodes r is)
