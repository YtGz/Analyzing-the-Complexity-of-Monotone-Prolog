module TRS.TPDB where

import Data.List (nub)
import ExprToTerm.Conversion
import Data.Rewriting.Rules (vars)
import Data.Rewriting.Rule (prettyRule)
import Text.PrettyPrint.ANSI.Leijen (text, putDoc, (<$$>), vcat)

printInTPDBFormat :: [Rule'] -> IO ()
printInTPDBFormat rules =
    putDoc
        (text ("(VAR" ++ concatMap (" " ++) (nub (vars rules)) ++ ")") <$$>
         text "(RULES" <$$>
         vcat (map (prettyRule (text "->") text text) rules) <$$>
         text ")")
