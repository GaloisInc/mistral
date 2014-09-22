{-# LANGUAGE DeriveDataTypeable #-}

module Mistral.Utils.Panic ( panic ) where

import qualified Control.Exception as X
import           Data.Typeable ( Typeable )


data Panic = Panic { panicSource :: String
                   , panicLines  :: [String]
                   } deriving (Typeable)

instance X.Exception Panic

instance Show Panic where
  show p = unlines
    [ "You have encountered a bug in the mistral compiler."
    , ""
    , "--%<--------------------------------------------------------------------"
    , locLab ++ panicSource p
    , msgLab ++ unlines messageLines
    , "--%<--------------------------------------------------------------------"
    , ""
    ]
    where
    locLab = "  Location : "
    msgLab = "  Message  : "
    msgPad = replicate (length msgLab) ' '
    messageLines = case panicLines p of
      l:ls -> l : map (msgPad ++) ls
      _    -> []


panic :: String -> [String] -> a
panic src msg = X.throw Panic { panicSource = src
                              , panicLines  = msg
                              }
