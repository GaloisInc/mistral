{-# LANGUAGE OverloadedStrings #-}

module Mistral.Parser.Unlit (
    Block(..)
  , unlit
  , markdown
  ) where

import qualified Data.Text.Lazy as L


data Block = Code    [L.Text]
           | Comment [L.Text]
             deriving (Show)

unlitBlock :: Block -> [L.Text]
unlitBlock b = case b of
  Code t     -> t
  Comment ls -> map (const L.empty) ls


-- | Collapse a list of blocks into a single source, removing comments and
-- keeping code.
unlit :: [Block] -> L.Text
unlit  = L.unlines . concatMap unlitBlock


-- | Turn text into a sequence of comment and code blocks.
markdown :: L.Text -> [Block]
markdown  = blanks [] . L.lines
  where

  blanks acc (l : ls)
    | isFence l = Comment (reverse (l:acc)) : code [] ls
    | isBlank l = blanks  (l:acc) ls
    | otherwise = comment (l:acc) ls
  blanks acc [] = [Comment (reverse acc)]

  comment acc (l : ls)
    | isBlank l  = blanks  (l:acc) ls
    | otherwise  = comment (l:acc) ls
  comment acc [] = [Comment (reverse acc)]

  code acc (l:ls)
    | isFence l = Code (reverse acc) : blanks [l] ls
    | otherwise = code (l:acc) ls
  code acc []   = [Code (reverse acc)]

  isBlank = L.null . L.strip
  isFence = (== "```") . L.strip
