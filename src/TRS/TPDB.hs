{-# OPTIONS_GHC -Wall #-}
module TRS.TPDB where

import Data.List (nub)
import System.IO
import ExprToTerm.Conversion
import Data.Rewriting.Rules (vars)
import Data.Rewriting.Rule (prettyRule)
import Text.PrettyPrint.ANSI.Leijen
       (Doc, text, hPutDoc, (<$$>), vcat)

saveFileInTPDBFormat :: String -> [Rule'] -> IO ()
saveFileInTPDBFormat filepath rules =
    withFile
        filepath
        WriteMode
        (`hPutDoc` (text
                        ("(VAR" ++ concatMap (" " ++) (nub (vars rules)) ++ ")") <$$>
                    text "(RULES" <$$>
                    vcat (map (prettyRule (text "->") (text . concat . words) text) rules) <$$>
                    text ")"))

concatSaveFileInTPDBFormat :: String -> [[Rule']] -> IO ()
concatSaveFileInTPDBFormat filepath rules =
    openFile filepath WriteMode >>= hClose >>
    withFile
        filepath
        AppendMode
        (`hPutDoc` vcat (map getDocInTPDBFormat rules))

getDocInTPDBFormat :: [Rule'] -> Doc
getDocInTPDBFormat rules =
    text ("(VAR" ++ concatMap (" " ++) (nub (vars rules)) ++ ")") <$$>
    text "(RULES" <$$>
    vcat (map (prettyRule (text "->") (text . concat . words) text) rules) <$$>
    text ")"
