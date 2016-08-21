module SymbolicEvaluationGraphs.Visualization where

import ExprToTerm.Conversion
import Data.Rewriting.Term.Type (Term(..))
import SymbolicEvaluationGraphs.Types
import SymbolicEvaluationGraphs.Utilities (splitClauseBody)
import SymbolicEvaluationGraphs.Heuristic (getInstanceCandidates)
import Data.Rewriting.Substitution (unify, apply)
import Data.Rewriting.Substitution.Type (toMap, fromMap)
import Data.Rewriting.Term (vars)
import Data.Map (toList, fromList)
import Data.Maybe
import Data.List (nubBy, nub, (\\))
import Data.Function (on)
import Data.String.Utils
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Layout.Tree
import Graphics.SVGFonts

--TODO: draw dashed arrows to instance father for instance rule
printSymbolicEvaluationGraph
    :: BTree (AbstractState, (String, Int)) -> IO ()
printSymbolicEvaluationGraph t =
    mainWith
        ((renderTree'
              fst
              (\((n,s),p1) ((_,s'),p2) ->
                    let rule = write (fst (snd s))
                        additionToU =
                            if fst (snd s) == "eval" && fst (unp2 p2) >=
                               fst (unp2 p1)
                                then write (getAdditionToU (fst s))
                                else write ""
                        mu =
                            if fst (snd s') == "instanceChild"
                                then write
                                         (showSubst'
                                              ((\(_,mu,_) ->
                                                     mu)
                                                   (head (fst (fst s')))))
                                else write ""
                        annotationToTheRight = additionToU `atop` mu
                    in rule #
                       translate
                           (((p1 .-. origin) ^+^ ((p2 .-. p1) ^* 0.5)) ^+^
                            (negated (V2 (snd (fromJust (extentX rule))) 0) ^+^
                             unit_X)) `atop`
                       annotationToTheRight #
                       translate
                           (((p1 .-. origin) ^+^ ((p2 .-. p1) ^* 0.5)) ^+^
                            (negated
                                 (V2
                                      (fst
                                           (fromJust
                                                (extentX annotationToTheRight)))
                                      0) #
                             scale 1.2)) `atop`
                       p1 ~~
                       p2)
              (fromJust
                   (symmLayoutBin'
                        (with & slVSep .~ 4 & slWidth .~ fromMaybe (0, 0) .
                         extentX .
                         fst &
                         slHeight .~
                         fromMaybe (0, 0) .
                         extentY .
                         fst)
                        (fmap
                             (\s ->
                                   let t =
                                           printAbstractState
                                               (fst
                                                    ((if fst (snd s) ==
                                                         "instanceChild"
                                                          then (\(([(t,_,c)],g),r) ->
                                                                     ( ( [ ( t
                                                                           , fromMap
                                                                                 (fromList
                                                                                      [])
                                                                           , c)]
                                                                       , g)
                                                                     , r))
                                                          else id)
                                                         s))
                                   in ( t `atop`
                                        rect
                                            (width t + (height t * 1.8))
                                            (height t + (height t * 1.8)) #
                                        fc white #
                                        lwL 0.2
                                      , s))
                             t))) #
          lwL 0.2 #
          centerXY #
          padX 1.1 #
          padY 1.4 #
          scale 15) :: Diagram B)

getAdditionToU :: AbstractState -> String
getAdditionToU ((t:_,_,Just (h,_)):_,_) = showTerm' t ++ " !~ " ++ showTerm' h
getAdditionToU s = error "Malformed abstract state."

printAbstractState :: AbstractState -> QDiagram B V2 Double Any
printAbstractState ([],_) = write "e"
printAbstractState (gs,(g,u)) =
    (foldr
         ((|||) . (||| (strutX 1.2 ||| write "|" ||| strutX 1.2)) . f)
         mempty
         (init gs) |||
     f (last gs)) #
    centerXY
  where
    f (qs,sub,clause) =
        write
            (if null qs
                 then ""
                 else concatMap ((++ ", ") . (`showTermWithG` g)) (init qs) ++
                      showTermWithG (last qs) g) |||
        (writeSubscript (showSubst' sub) `atop`
         writeSuperscript (showClause clause))

write :: String -> QDiagram B V2 Double Any
write s =
    stroke (textSVG' (TextOpts bit INSIDE_H KERN False 1 1) s) # fc black #
    lwL 0.00002

writeSubscript :: String -> QDiagram B V2 Double Any
writeSubscript s = strutX 0.2 ||| write s # translateY (-0.55) # scale 0.55

writeSuperscript :: String -> QDiagram B V2 Double Any
writeSuperscript s = strutX 0.2 ||| write s # translateY 0.55 # scale 0.55

showTerm' :: Term' -> String
showTerm' (Fun f []) = replace "Left " "" f
showTerm' (Fun f args) =
    replace "Left " "" f ++ "(" ++ concatMap ((++ ",") . showTerm') (init args) ++
    showTerm' (last args) ++
    ")"
showTerm' (Var v) = v

showTermWithG :: Term' -> G -> String
showTermWithG (Fun f []) _ = replace "Left " "" f
showTermWithG (Fun f args) g =
    replace "Left " "" f ++ "(" ++
    concatMap ((++ ",") . (`showTermWithG` g)) (init args) ++
    showTermWithG (last args) g ++
    ")"
showTermWithG (Var v) g =
    if Var v `elem` g
        then "^" ++ v
        else v

showSubst' :: Subst' -> String
showSubst' sub =
    if null ls
        then ""
        else "{" ++ concatMap ((++ ", ") . f) (init ls) ++ f (last ls) ++ "}"
  where
    f (v,t) = v ++ " -> " ++ showTerm' t
    ls =
        filter
            (\(v,t) ->
                  Var v /= t)
            (toList (toMap sub))

showClause :: Maybe Clause -> String
showClause Nothing = ""
showClause (Just (h,b)) =
    showTerm' h ++ " :- " ++
    if isNothing b
        then ""
        else let bs = splitClauseBody (fromJust b)
             in concatMap ((++ ", ") . showTerm') (init bs) ++
                showTerm' (last bs)
