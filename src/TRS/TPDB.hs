module TRS.TPDB where

import Data.List (nub)
import System.IO
import ExprToTerm.Conversion
import Data.Rewriting.Rules (vars)
import Data.Rewriting.Rule (prettyRule)
import Text.PrettyPrint.ANSI.Leijen (text, hPutDoc, (<$$>), vcat)

printInTPDBFormat :: String -> [Rule'] -> IO ()
printInTPDBFormat filepath rules =
    withFile filepath WriteMode (`hPutDoc`
        (text ("(VAR" ++ concatMap (" " ++) (nub (vars rules)) ++ ")") <$$>
         text "(RULES" <$$>
         vcat (map (prettyRule (text "->") text text) rules) <$$>
         text ")"))
