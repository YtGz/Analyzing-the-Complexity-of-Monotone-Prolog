module TRS.Encoding where

import Control.Monad.State
import SymbolicEvaluationGraphs.Types (AbstractState, G)
import SymbolicEvaluationGraphs.InferenceRules (nextG)
import ExprToTerm.Conversion (Term')
import Data.List (intersect)
import Data.Rewriting.Term (Term(..), vars)
import Data.Rewriting.Substitution (apply)
import Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty))

encodeIn :: BTree (AbstractState, String) -> State Int Term'
encodeIn (BNode (([(ts,_,_)],(g,_)),"instance") l@(BNode (([(_,mu,_)],_),"instanceChild") Empty Empty) _) =
    fmap (apply mu) (encodeIn l)
encodeIn (BNode (([(ts,_,_)],(g,_)),_) _ _) = do
    i <- get
    modify (+ 1)
    return
        (Fun ("fin" ++ show i) (concatMap (map Var . vars) ts `intersect` g))
encodeIn _ = error "Cannot encode abstract state: multiple goals."

encodeOut :: BTree (AbstractState, String) -> StateT Int IO Term'
encodeOut (BNode (([(ts,_,_)],(g,_)),"instance") l@(BNode (([(_,mu,_)],_),"instanceChild") Empty Empty) _) =
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
