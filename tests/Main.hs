module Main where


import Discover ( discoverTests )
import Options ( parseOptions, testRunnerOpts )

import Test.Framework ( Test )
import Test.Framework.Runners.Console ( defaultMainWithOpts )


main :: IO ()
main  =
  do opts       <- parseOptions
     discovered <- discoverTests opts
     defaultMainWithOpts (staticTests ++ discovered) (testRunnerOpts opts)

staticTests :: [Test]
staticTests  = []
