{-# OPTIONS_GHC -Wall   #-}
module SymbolicEvaluationGraphs.Visualization where

import Control.Concurrent (threadDelay, forkIO, killThread)
import Data.Function (fix)
import Data.Map (toList, fromList)
import Data.Maybe
import Data.String.Utils
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import Data.Rewriting.Substitution.Type (toMap, fromMap)
import Data.Rewriting.Term.Type (Term(..))
import Diagrams.Backend.SVG (B, renderSVG)
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Graphics.SVGFonts

import ExprToTerm.Conversion
import SymbolicEvaluationGraphs.Types
import SymbolicEvaluationGraphs.Utilities (splitClauseBody)

--TODO: draw dashed arrows to instance father for instance rule
printSymbolicEvaluationGraph
    :: FilePath -> BTree (AbstractState, (String, Int)) -> IO ()
printSymbolicEvaluationGraph filepath tree = do
    let diagram =
            (renderTree'
                 fst
                 (\((_,s),pt1) ((_,s'),pt2) ->
                       let rule = write (fst (snd s))
                           additionToU =
                               if fst (snd s) == "eval" && fst (unp2 pt2) >=
                                  fst (unp2 pt1)
                                   then write (getAdditionToU (fst s))
                                   else write ""
                           mu =
                               if fst (snd s') == "instanceChild"
                                   then write
                                            (showSubst'
                                                 ((\(_,mu',_) ->
                                                        mu')
                                                      (head (fst (fst s')))))
                                   else write ""
                           annotationToTheRight = additionToU `atop` mu
                       in rule #
                          translate
                              (((pt1 .-. origin) ^+^ ((pt2 .-. pt1) ^* 0.5)) ^+^
                               (negated (V2 (snd (fromJust (extentX rule))) 0) ^+^
                                unit_X)) `atop`
                          annotationToTheRight #
                          translate
                              (((pt1 .-. origin) ^+^ ((pt2 .-. pt1) ^* 0.5)) ^+^
                               (negated
                                    (V2
                                         (fst
                                              (fromJust
                                                   (extentX
                                                        annotationToTheRight)))
                                         0) #
                                scale 1.2)) `atop`
                          pt1 ~~
                          pt2)
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
                                                  ((if fst (snd s) ==
                                                       "instanceChild"
                                                        then (\(([(t',_,c)],g),r) ->
                                                                   ( ( [ ( t'
                                                                         , fromMap
                                                                               (fromList
                                                                                    [])
                                                                         , c)]
                                                                     , g)
                                                                   , r))
                                                        else id)
                                                       s)
                                      in ( t `atop`
                                           rect
                                               (width t + (height t * 1.8))
                                               (height t + (height t * 1.8)) #
                                           fc white #
                                           if fst (snd s) == "split"
                                               then lc crimson # lwL 0.4
                                               else lwL 0.2
                                         , s))
                                tree))) #
             lwL 0.2 #
             centerXY #
             padX 1.1 #
             padY 1.4 #
             scale 15) :: Diagram B
    putStrLn ""
    putStrLn "generating the vector graphic..."
    hSetBuffering stdout NoBuffering
    animThread <-
        forkIO
            (fix
                 (\f i c ->
                       putChar c >> threadDelay 20000 >>
                       if i == (19 :: Integer)
                           then putChar '\r' >>
                                f
                                    0
                                    (if c == '#'
                                         then '>'
                                         else '#')
                           else f (i + 1) c)
                 0
                 '#')
    renderSVG filepath (mkSizeSpec2D (Just 3400.0) (Just 3400.0)) diagram
    killThread animThread
    putChar '\r'
    putStr (replicate 20 ' ')
    putChar '\r'
    putStrLn "done!"
    putStrLn ""
    putStrLn ""

getAdditionToU :: AbstractState -> String
getAdditionToU ((t:_,_,Just (h,_)):_,_) = showTerm' t ++ " !~ " ++ showTerm' h
getAdditionToU _ = error "Malformed abstract state."

printAbstractState :: (AbstractState, (String, Int)) -> QDiagram B V2 Double Any
printAbstractState (([],_),_) = write "e"
printAbstractState ((gs,(g,_)),(s,i)) =
    labelNode
        (foldr
             ((|||) . (||| (strutX 1.2 ||| write "|" ||| strutX 1.2)) . f)
             mempty
             (init gs) |||
         f (last gs)) #
    centerXY
  where
    labelNode =
        if s == "split"
            then (\x ->
                       x ||| (strutX 1.2 ||| write "||" ||| strutX 1.2) |||
                       write (show i))
            else id
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
