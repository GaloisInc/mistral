{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Mistral.Utils.Source where

import Control.Applicative ((<|>),pure,(<$>))
import Data.Foldable (Foldable,foldMap)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable)

import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )


-- Positions -------------------------------------------------------------------

data Position = Position { posRow, posCol, posOff :: !Int
                         } deriving (Show,Generic)

instance Serialize Position

instance Eq Position where
  (==) = (==) `on` posOff
  (/=) = (/=) `on` posOff

instance Ord Position where
  compare = compare `on` posOff

moveChar :: Char -> Position -> Position
moveChar c p
  | c == '\n' = p { posRow = posRow p + 1, posCol = 1, posOff = posOff p + 1 }
  | c == '\t' = p { posCol = posCol p + 8,             posOff = posOff p + 8 }
  | otherwise = p { posCol = posCol p + 1,             posOff = posOff p + 1 }

moveString :: String -> Position -> Position
moveString cs p = foldl (flip moveChar) p cs


-- Ranges ----------------------------------------------------------------------

data Range = Range { rangeStart, rangeEnd :: !Position
                   } deriving (Show,Eq,Ord,Generic)

instance Serialize Range

-- | Merge two ranges into one larger range
rComb :: Range -> Range -> Range
rComb l r = Range { rangeStart = (min `on` rangeStart) l r
                  , rangeEnd   = (max `on` rangeEnd)   l r
                  }


-- Source Locations ------------------------------------------------------------

data Source = Unknown
              -- ^ No location information given
            | NoSource        !Range
              -- ^ Unknown source, but range information available
            | Source FilePath !Range
              -- ^ Known source and range
              deriving (Show,Eq,Ord,Generic)

instance Serialize Source

instance Monoid Source where
  mempty = Unknown

  mappend Unknown r       = r
  mappend l       Unknown = l

  mappend l r = fromMaybe Unknown $ do
    lr <- range l
    rr <- range r

    -- prefer the source from the left
    mk <- Source <$> source l
      <|> Source <$> source r
      <|> pure NoSource

    return (mk (rComb lr rr))

source :: Source -> Maybe FilePath
source loc = case loc of
  Source path _ -> Just path
  _             -> Nothing

range :: Source -> Maybe Range
range loc = case loc of
  Source _ r -> Just r
  NoSource r -> Just r
  _          -> Nothing


-- Located Things --------------------------------------------------------------

data Located a = Located { locSource :: Source
                         , locThing  :: a
                         } deriving (Functor,Foldable,Traversable,Show)

instance Eq a => Eq (Located a) where
  (==) = (==) `on` locThing
  (/=) = (/=) `on` locThing

instance Ord a => Ord (Located a) where
  compare = compare `on` locThing


at :: a -> Source -> Located a
a `at` src = Located { locSource = src
                     , locThing  = a }

class HasSource a where
  getSource :: a -> Source

instance HasSource Source where
  getSource = id

instance HasSource a => HasSource (Maybe a) where
  getSource = foldMap getSource

instance HasSource a => HasSource [a] where
  getSource = foldMap getSource

instance HasSource (Located a) where
  getSource = locSource



-- | Erase Source Annotations
class NoSource a where
  noSource :: a -> a

instance NoSource Source where
  noSource _ = Unknown

instance NoSource (Located a) where
  noSource loc = loc { locSource = Unknown }

instance NoSource a => NoSource (Maybe a) where
  noSource = fmap noSource

instance NoSource a => NoSource [a] where
  noSource = fmap noSource
