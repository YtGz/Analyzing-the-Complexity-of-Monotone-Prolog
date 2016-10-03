{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types, UndecidableInstances #-}
module Main where

import           System.Environment
import           System.Exit
import           System.IO
import           System.Console.GetOpt
import           Language.Prolog.Parser
import           Query.Utilities
import           SymbolicEvaluationGraphs.Visualization
import           SymbolicEvaluationGraphs.Heuristic
import           TRS.Encoding hiding (getNode)
import           TRS.TPDB
import           Data.Map
import qualified Data.List
import           Control.Monad.State
import           Diagrams.TwoD.Layout.Tree (BTree(BNode, Empty))


data Options = Options  { optInputPath        :: FilePath
                        , optTRSOutputPath    :: FilePath
                        , optGraphOutputPath  :: FilePath
                        , optForceGraphOutput :: Bool
                        }

defaultOptions :: Options
defaultOptions = Options  { optInputPath        = []
                          , optTRSOutputPath    = []
                          , optGraphOutputPath  = []
                          , optForceGraphOutput = False
                          }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInputPath = arg })
            "FILE")
        "Path to input file (required)"

    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optTRSOutputPath = arg })
            "FILE")
        "Path to TRS output file (required)"

    , Option "g" ["graph"]
        (ReqArg
            (\arg opt -> return opt { optGraphOutputPath = arg })
            "FILE")
        "Path to graph output file (required)"

    , Option "f" ["force"]
        (NoArg
            (\opt -> return opt { optForceGraphOutput = True }))
        "Force graph output"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
              prg <- getProgName
              hPutStrLn stderr (usageInfo prg options)
              exitSuccess))
        "Show help"
    ]

checkRequiredOpts :: Options -> IO ()
checkRequiredOpts opts = when (any Data.List.null [optInputPath opts, optTRSOutputPath opts, optGraphOutputPath opts]) (error "Missing arguments. Type --help for help.")

main :: IO ()
main = do
  args <- getArgs
  let (actions, _, errors) = getOpt RequireOrder options args
  unless (Data.List.null errors) (mapM_ (hPutStrLn stderr) errors >> exitFailure)
  opts <- Data.List.foldl (>>=) (return defaultOptions) actions
  checkRequiredOpts opts
  (exprs,_) <- parseProlog2 (optInputPath opts)
  putStrLn ""
  let queryClasses = getQueryClasses exprs
  when (Data.List.null queryClasses) (
    putStrLn "Input program contains no queries." >>
    putStrLn "No analysis required." >>
    exitSuccess)
  queryClass <- if length queryClasses > 1 then do
        putStrLn "Multiple query classes detetected:"
        putStrLn ""
        mapM_ putStrLn (zipWith (\i c -> show i ++ ": " ++ show c) [0 :: Integer ..] queryClasses)
        putStrLn ""
        putStrLn "Please indicate which one to analyze."
        i <- readLn
        putStrLn ""
        return (queryClasses !! i)
      else
        return (head queryClasses)
  ((graph, groundnessAnalysisInformation), generalizationInformation) <- runStateT (runStateT (generateSymbolicEvaluationGraph queryClass) Data.Map.empty) Data.Map.empty
  if not (Data.List.null (fix (\f n ->
        case n of
            BNode (_,(s,_)) l r ->
                [ s
                | s == "split" ] ++
                f l ++ f r
            Empty -> []) graph)) then do
    printSymbolicEvaluationGraph (optGraphOutputPath opts) graph
    putStrLn "Please take a look at the generated graph. Are there multiplicative split nodes (y/n)? Potential nodes are marked in red."
    ans <- getLine
    putStrLn ""
    if ans == "y" || ans == "yes" then do
      putStrLn "Please indicate the positions of the multiplicative split nodes:"
      mulSplitNodes <- readLn
      putStrLn ""
      putStrLn "Please note that this will fail if the symbolic evaluation graph is not decomposable."
      putStrLn ""
      putStrLn ""
      rewriteRules <- evalStateT (evalStateT (generateRewriteRulesForGraphsWithMultSplitNodes graph mulSplitNodes) groundnessAnalysisInformation) generalizationInformation
      concatSaveFileInTPDBFormat (optTRSOutputPath opts) rewriteRules
    else do
      rewriteRules <- evalStateT (evalStateT (generateRewriteRules graph) groundnessAnalysisInformation) generalizationInformation
      saveFileInTPDBFormat (optTRSOutputPath opts) rewriteRules
  else do
    when (optForceGraphOutput opts) (printSymbolicEvaluationGraph (optGraphOutputPath opts) graph)
    rewriteRules <- evalStateT (evalStateT (generateRewriteRules graph) groundnessAnalysisInformation) generalizationInformation
    saveFileInTPDBFormat (optTRSOutputPath opts) rewriteRules
