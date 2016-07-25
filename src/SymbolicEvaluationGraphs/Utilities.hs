module SymbolicEvaluationGraphs.Utilities
  (freshVariable
  ,isAbstractVariable
  ,hasNoAbstractVariables
  ,termToClause
  ,getInitialAbstractState
  ,printSymbolicEvaluationGraph
  ,showTerm')
  where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import ExprToTerm.Conversion
import Data.Rewriting.Term.Type (Term(..))
import SymbolicEvaluationGraphs.Types
import Query.Utilities
import Data.Rewriting.Substitution.Type (fromMap, toMap)
import Data.Rewriting.Term (vars)
import Data.Map (fromList, toList)
import Text.Read (readMaybe)
import Data.Maybe
import Data.String.Utils
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Layout.Tree
import Graphics.SVGFonts

counter :: IORef Int
{-# NOINLINE counter #-}

counter = unsafePerformIO (newIORef 0)

freshVariable :: Term' -> Term' -- TODO: eliminate impurity introduced by using unsafePerformIO
freshVariable _ =
    Var
        ("T" ++
         show
             (unsafePerformIO
                  (atomicModifyIORef
                       counter
                       (\x ->
                             (x + 1, x)))))

-- abstract variables have the format "T" ++ [Int]
isAbstractVariable
    :: String -> Bool
isAbstractVariable (x:xs) = x == 'T' && isJust (readMaybe xs :: Maybe Int)

-- check if no variable in the input program is of the format of our abstract variables
hasNoAbstractVariables
    :: [Clause] -> Bool
hasNoAbstractVariables clauses =
    let f t = not (any isAbstractVariable (vars t))
    in all
           (\(t,ts) ->
                 f t && all f ts)
           clauses

termToClause :: Term' -> Clause
termToClause (Fun ":-" args) = (head args, tail args) -- rule
termToClause h@(Fun _ _) = (h, []) -- fact (empty body)
termToClause _ = error "Cannot apply 'exprToClause': Malformed Clause"

getInitialAbstractState :: QueryClass -> AbstractState
getInitialAbstractState (f,m) =
    ([([Fun f vs], fromMap (fromList []), Nothing)], (gs, []))
  where
    (vs,gs) =
        foldr
            (\x (vs,gs) ->
                  let v = freshVariable (Var "")
                  in case x of
                         In -> (v : vs, v : gs)
                         Out -> (v : vs, gs))
            ([], [])
            m

printSymbolicEvaluationGraph :: BTree AbstractState -> IO ()
printSymbolicEvaluationGraph t =
    mainWith
        ((renderTree'
              id
              (\(s,p1) (_,p2) ->
                    p1 ~~ p2)
              (fromJust
                   (symmLayoutBin'
                        (with & slVSep .~ 4 & slWidth .~ fromMaybe (0, 0) .
                         extentX &
                         slHeight .~
                         fromMaybe (0, 0) .
                         extentY)
                        (fmap
                             (\s ->
                                   let t = printAbstractState s
                                   in t `atop`
                                      rect
                                          (width t + (height t * 1.8))
                                          (height t + (height t * 1.8)) #
                                      fc white #
                                      lwL 0.2)
                             t))) #
          lwL 0.2 #
          centerXY #
          padX 1.1 #
          padY 1.4 #
          scale 15) :: Diagram B)

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
                 else concatMap ((++ ", ") . showTerm') (init qs) ++
                      showTerm' (last qs)) |||
        (writeSubscript (showSubst' sub) `atop` writeSuperscript (showClause clause))

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
showClause (Just (h,bs)) =
    showTerm' h ++ " :- " ++
    if null bs
        then ""
        else concatMap ((++ ", ") . showTerm') (init bs) ++ showTerm' (last bs)
