module Options (
    Options(..)
  , parseOptions
  , testRunnerOpts
  ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Data.Monoid ( Monoid(..), mconcat )
import System.Console.GetOpt ( ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt
                             , usageInfo )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Test.Framework.Runners.Options ( RunnerOptions, RunnerOptions'(..)
                                      , ColorMode(..) )


-- Options Parsing -------------------------------------------------------------

data Setter a = Setter (a -> a)
              | Error [String]

instance Monoid (Setter a) where
  mempty        = Setter id
  mappend l r   = case (l,r) of
                    (Setter f, Setter g) -> Setter (f . g)
                    (Error ls, Error rs) -> Error (ls ++ rs)
                    (Error{},  _       ) -> l
                    (_ ,       Error{} ) -> r

data Options = Options { optTestFiles     :: [FilePath]
                       , optReportFile    :: Maybe FilePath
                       , optHideSuccesses :: Bool
                       , optHelp          :: Bool
                       } deriving (Show)

initialOptions :: Options
initialOptions  = Options { optTestFiles     = []
                          , optReportFile    = Nothing
                          , optHideSuccesses = False
                          , optHelp          = False
                          }

testRunnerOpts :: Options -> RunnerOptions
testRunnerOpts opts =
  mempty { ropt_hide_successes = Just (optHideSuccesses opts)
         , ropt_xml_output     = Just       <$> optReportFile opts
         , ropt_xml_nested     = const True <$> optReportFile opts
         , ropt_color_mode     = Just ColorAuto
         }


addTest :: FilePath -> Setter Options
addTest path =
  Setter (\opts -> opts { optTestFiles = optTestFiles opts ++ [path] })

setReportFile :: FilePath -> Setter Options
setReportFile path =
  Setter (\opts -> opts { optReportFile = Just path })

setHideSuccesses :: Setter Options
setHideSuccesses  = Setter (\opts -> opts { optHideSuccesses = True })

setHelp :: Setter Options
setHelp  = Setter (\opts -> opts { optHelp = True })

options :: [OptDescr (Setter Options)]
options  =
  [ Option "r" ["report-xml"] (ReqArg setReportFile "FILEPATH")
    "file to write junit-xml results to"

  , Option "" ["hide-successes"] (NoArg setHideSuccesses)
    "only show test failures"

  , Option "h" ["help"] (NoArg setHelp)
    "display this message"

  ]

parseOptions :: IO Options
parseOptions  =
  do args <- getArgs
     case getOpt (ReturnInOrder addTest) options args of
       (ps,_,[])  -> validate (mconcat ps)
       (_,_,errs) -> do printUsage errs
                        exitFailure


validate :: Setter Options -> IO Options
validate ps = case ps of

  Setter f ->
    do let opts = f initialOptions

       when (optHelp opts) $
         do printUsage []
            exitSuccess

       return opts

  Error errs ->
    do printUsage errs
       exitFailure


printUsage :: [String] -> IO ()
printUsage errs =
  do name <- getProgName
     let banner = unlines
                $ errs ++ ["Usage: ./" ++ name ++ " [OPTIONS] test1 .."]
     putStrLn (usageInfo banner options)
