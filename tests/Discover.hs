{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Discover where

import Options ( Options(..) )

import Mistral.Driver ( runDriver, tryMessages )
import Mistral.Parser ( parseModule )
import Mistral.Utils.PP ( pretty, pp, vcat )

import Control.Applicative ( (<$>) )
import Control.Monad ( guard, msum, mplus )
import Data.List ( isPrefixOf, isSuffixOf, nub )
import Data.Monoid ( Monoid(..) )
import Data.Typeable ( Typeable )
import System.Directory
       ( doesDirectoryExist, doesFileExist, getDirectoryContents )
import System.FilePath ( (</>), (<.>), takeFileName, dropFileName )
import Test.Framework ( testGroup )
import Test.Framework.Providers.API
       ( Test(..), TestResultlike(..), Testlike(..), (:~>)(..) )
import Test.Golden ( goldenVsStringDiff )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import qualified Data.Text.Lazy.IO    as L


discoverTests :: Options -> IO [Test]
discoverTests opts =
  do groups <- mapM gatherTests (optTestFiles opts)
     return (concatMap createTests groups)


-- Mistral Tests ---------------------------------------------------------------

data TestType = Load FilePath
                -- ^ The source parses, and raises no errors.  Warnings will be
                -- ignored.
              | ExpectFail FilePath FilePath
                -- ^ An expected failure, with the expected output.
                deriving (Show,Eq)

data Tests = Tests { tSub   :: Map.Map String Tests
                   , tTests :: [TestType]
                   } deriving (Show)

instance Monoid Tests where
  mempty      = Tests { tSub   = Map.empty
                      , tTests = []
                      }
  mappend l r = Tests { tSub   = Map.unionWith mappend (tSub l) (tSub r)
                      , tTests = nub (tTests l ++ tTests r)
                      }

singleton :: TestType -> Tests
singleton t = Tests Map.empty [t]

addPrefix :: FilePath -> Tests -> Tests
addPrefix path tests = Tests (Map.singleton path tests) []

createTests :: Tests -> [Test]
createTests  = loop ""
  where
  loop base t
    | null (tTests t) = subs
    | otherwise       = [testGroup base (group ++ subs)]
    where
    group = [ parseTest test | test <- (tTests t) ]
    subs  = concat [ loop (base </> p) gt' | (p,gt') <- Map.toList (tSub t) ]


-- Parse Tests -----------------------------------------------------------------

parseTest :: TestType -> Test
parseTest t = case t of

  Load path -> Test path (ParseTest path)

  ExpectFail path gold -> goldenVsStringDiff path mkDiff gold $
    do bytes     <- L.readFile path
       (es,ws,_) <- runDriver (tryMessages (parseModule (Just path) bytes))
       let out = fromString (pretty (vcat (map pp es ++ map pp ws)) ++ "\n")
       return out

    where
    mkDiff ref new = ["diff", "-u", ref, new]

    fromString :: String -> BL.ByteString
    fromString  = BL.pack . map (toEnum . fromEnum)



data Result = Success
            | Failure String

instance Show Result where
  show r = case r of
    Success     -> "OK"
    Failure msg -> msg

instance TestResultlike () Result where
  testSucceeded r = case r of
    Success -> True
    _       -> False

newtype ParseTest = ParseTest FilePath
                    deriving (Typeable)

instance Testlike () Result ParseTest where

  runTest _opts (ParseTest path) =
    do bytes <- L.readFile path
       res   <- runDriver (tryMessages (parseModule (Just path) bytes))
       let val = case res of
                   ([],[],Just _) -> Success
                   (es,ws,_)      -> Failure $ unlines
                                             $ map pretty ws ++ map pretty es
       return (Finished val, return ())


  testTypeName _ = "Parse Test"


-- Filesystem Discovery --------------------------------------------------------

gatherTests :: FilePath -> IO Tests
gatherTests path
  | null base =                    gatherTests' path
  | otherwise = addPrefix base <$> gatherTests' path
  where
  base = dropFileName path

gatherTests' :: FilePath -> IO Tests
gatherTests' path =
  do isDir  <- doesDirectoryExist path
     if isDir
        then gatherDir path
        else addTest path

addTest :: FilePath -> IO Tests
addTest path = isTest `mplus` return mempty
  where
  file = takeFileName path

  isTest =
    do isFile <- doesFileExist path
       guard (isFile && ".mst" `isSuffixOf` file && isValidFile file)
       msum [ expectFailTest
            , shouldParseTest
            ]

  expectFailTest =
    do let err = path <.> "stderr"
       outExists <- doesFileExist err
       guard outExists
       return (singleton (ExpectFail path err))

  shouldParseTest =
    do return (singleton (Load path))


gatherDir :: FilePath -> IO Tests
gatherDir dirName =
  do entries <- getDirectoryContents dirName
     tests   <- mapM gatherTests' [ dirName </> file | file <- entries
                                                     , isValidFile file ]
     return (addPrefix (takeFileName dirName) (mconcat tests))


isValidFile :: FilePath -> Bool
isValidFile path
  | "." `isPrefixOf` path = False
  | otherwise             = True
