{-# LANGUAGE FlexibleContexts #-}

module Main where

import OptParser

import Mistral.Driver
import Mistral.Simple ( compileFile, linkProgram )
import Mistral.Utils.PP
import Mistral.Semantics.SimpleInterp as M


import Control.Monad ( when, unless )
import Data.Monoid ( mconcat, mappend )
import System.Console.GetOpt
           ( OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )


-- Options ---------------------------------------------------------------------

data Options = Options { optSources     :: [FilePath]
                       , optHelp        :: Bool
                       , optLayout      :: Bool
                       , optRealNames   :: Bool
                       , optTracePhases :: [String]
                       , optAction      :: Action
                       , optInterpret   :: Bool
                       } deriving (Show)

defaultOptions :: Options
defaultOptions  = Options { optSources     = []
                          , optHelp        = False
                          , optLayout      = False
                          , optRealNames   = False
                          , optTracePhases = []
                          , optAction      = Compile
                          , optInterpret   = False
                          }

data Action = Compile | Link deriving (Show,Eq)

optPPEnv :: Options -> PPEnv
optPPEnv opts = defaultPPEnv { peLayout    = optLayout opts
                             , peRealNames = optRealNames opts }

addSource :: String -> Parser Options
addSource str = success (\ opts -> opts { optSources = str:optSources opts })

setHelp :: Parser Options
setHelp  = success (\ opts -> opts { optHelp = True })

setPPArg :: String -> Parser Options
setPPArg "real-names" = setRealNames
setPPArg "layout"     = setLayout
setPPArg arg          = failure ("Unknown PP option: " ++ arg)

setLayout :: Parser Options
setLayout  = success (\ opts -> opts { optLayout = True })

setRealNames :: Parser Options
setRealNames  = success (\ opts -> opts { optRealNames = True })

setAction :: Action -> Parser Options
setAction act = success (\opts -> opts { optAction = act })

setInterpret :: Parser Options
setInterpret = success  (\ opts -> opts { optInterpret = True })
     `mappend` setAction Link

addTracePhase :: String -> Parser Options
addTracePhase n =
  success (\ opts -> opts { optTracePhases = phases n ++ optTracePhases opts })
  where
  phases str = case break (== ',') str of
    (xs,_:ys)          -> xs : phases ys
    (xs,_) | null xs   -> []
           | otherwise -> [xs]

options :: [OptDescr (Parser Options)]
options  =
  [ Option "h" ["help"] (NoArg setHelp)
    "Display this message"

  , Option "P" [] (ReqArg setPPArg "real-names,layout")
    "Configure the pretty-printer"

  , Option "" ["trace"] (ReqArg addTracePhase "PHASE")
    "Dump trace output for a specific phase"

  , Option "i" ["interpret"] (NoArg setInterpret)
      "Interpret the program"

  , Option "c" ["compile"] (NoArg (setAction Compile))
      "Compile a module"

  , Option "l" ["link"] (NoArg (setAction Link))
      "Link modules together"
  ]

getOptions :: IO Options
getOptions  = do
  args <- getArgs
  case getOpt (ReturnInOrder addSource) options args of
    (ps,[],[]) -> validate (mconcat ps)
    (_,_,errs) -> do printUsage errs
                     exitFailure

validate :: Parser Options -> IO Options
validate p =
  case p of
    Success apply ->
      do let opts = apply defaultOptions
         when (optHelp opts) $
           do printUsage []
              exitSuccess

         when (null (optSources opts)) $
           do printUsage ["No sources provided"]
              exitFailure

         return opts

    Err errs ->
      do printUsage errs
         exitFailure

printUsage :: [String] -> IO ()
printUsage errs = do
  prog <- getProgName
  putStrLn (usage errs prog options)


-- Main ------------------------------------------------------------------------

main :: IO ()
main  = do
  opts <- getOptions
  ok <- runDriverOpts (optPPEnv opts) (optTracePhases opts) rwBytesFS $
        phase "mistral" $ do

    (es,ws,_) <- tryMessages $ do

      -- XXX this should fail in a more graceful manner
      let [f] = optSources opts
      m <- compileFile f

      when (optAction opts == Link) $
        do prog <- linkProgram m

           -- XXX output a linked program

           -- XXX print the final result?
           when (optInterpret opts) $ do
             _ <- M.interpret prog
             return ()


    let dump x = do d <- ppM x
                    io (print d)

    mapM_ dump ws
    mapM_ dump es
    return (null es)

  unless ok exitFailure
