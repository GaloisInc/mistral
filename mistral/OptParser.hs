module OptParser where

import Data.Monoid ( Monoid(..) )
import System.Console.GetOpt ( OptDescr, usageInfo )


data Parser opt = Success (opt -> opt)
                | Err [String]

instance Monoid (Parser opt) where
  mempty                         = Success id
  Success f  `mappend` Success g = Success (f . g)
  Err ls     `mappend` Err rs    = Err (ls ++ rs)
  l@Err{}    `mappend` _         = l
  _          `mappend` r@Err{}   = r


success :: (opt -> opt) -> Parser opt
success  = Success

failure :: String -> Parser opt
failure msg = Err [msg]

usage :: [String] -> String -> [OptDescr a] -> String
usage msgs prog = usageInfo
                $ unlines (msgs ++ ["Usage: " ++ prog ++ " [OPTIONS] {file}*"])
