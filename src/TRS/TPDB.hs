{-# OPTIONS_GHC -Wall  #-}
module TRS.TPDB where

import Data.List (nub)
import System.IO

import Data.Rewriting.Rule (prettyRule)
import Data.Rewriting.Rules (vars)
import Text.PrettyPrint.ANSI.Leijen
       (Doc, text, hPutDoc, (<$$>), vcat)

import ExprToTerm.Conversion

saveFileInTPDBFormat :: String -> [Rule'] -> IO ()
saveFileInTPDBFormat filepath rules =
    withFile
        filepath
        WriteMode
        (`hPutDoc` (text
                        ("(VAR" ++ concatMap (" " ++) (nub (vars rules)) ++ ")") <$$>
                    text "(RULES" <$$>
                    vcat
                        (map
                             (prettyRule
                                  (text "->")
                                  (text . concat . words)
                                  text)
                             rules) <$$>
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
