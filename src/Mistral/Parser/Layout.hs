{-# LANGUAGE PatternGuards #-}

module Mistral.Parser.Layout ( layout ) where

import Mistral.Parser.LexerCore
                  ( Lexeme, Token(..), Symbol(..), Virt(..), Keyword(..)
                  , TokenError(..) )
import Mistral.Utils.Panic ( panic )
import Mistral.Utils.Source ( Located(..), at, range, Range(..), Position(..) )


data Block = Let | Virtual !Int | Explicit
    deriving (Show,Eq)

isVirtual :: Block -> Bool
isVirtual block = case block of
  Virtual{} -> True
  _         -> False

-- | Insert explicit virtual delimiters for scoping.
layout :: [Lexeme] -> [Lexeme]
layout  = loop False []
  where

  virt src v = Located { locSource = src, locThing = Virt v }

  col l = case range (locSource l) of
            Just r  -> posCol (rangeStart r)
            Nothing -> panic "Mistral.Parser.Layout" [ "unexpected empty source" ]

  beginsLayout t = case t of
    KW KW_where       -> True
    KW KW_let         -> True
    KW KW_topology    -> True
    KW KW_taskSet     -> True
    KW KW_schedule    -> True
    KW KW_actions     -> True
    KW KW_transitions -> True
    KW KW_of          -> True
    _                 -> False

  loop _ _ [] = missingEof

  -- close all open blocks
  loop startBlock stack (l @ Located { locSource = src, locThing = Eof } : _) =
    openClose ++ replicate (length stack) (virt src VClose) ++ [l]
    where
    openClose | startBlock = [ virt src VOpen, virt src VClose ]
              | otherwise  = []


  -- the previous token started a new block scope
  loop True stack (l @ Located { locThing = t } : ls)
      -- explicit block
    | Sym BraceL <- t = l : loop False (Explicit : stack) ls

      -- layout block
    | otherwise       = virt (locSource l) VOpen
                        : l : loop (beginsLayout t) (Virtual (col l) : stack) ls


  loop _ stack (l @ Located { locSource = src, locThing = t } : ls)

      -- don't perform layout once a lexical error is found
    | Err _ <- t = [l]

      -- `let` is a weird special case, catch it and transition to starting a
      -- layout block
    | KW KW_let <- t = l : loop True (Let:stack) ls

      -- `in` closes all blocks up to the last `Let` 
    | KW KW_in <- t = case closeLet src stack of
        Just (toks,stack') -> toks ++ l : loop False stack' ls
        Nothing            -> [Err LexicalError `at` src]

      -- emit the token, and begin a layout block
    | beginsLayout t = sepToks ++ loop True stack ls

      -- special case for an action block with no actions keyword
    | Sym BraceL <- t = l : loop False (Explicit : stack) ls

    where

    sepToks = case stack of
      Virtual c : _ | col l == c -> [ virt src VSep , l ]
      _                          -> [ l ]

  -- punctuate or close virtual blocks
  loop _ stack@(p : ps) ts@(l @ Located { locThing = t } : ls) = case p of
    Virtual c
      | col l == c -> virt (locSource l) VSep : l : loop False stack ls
      | col l <  c -> virt (locSource l) VClose :     loop False ps ts

    Explicit
      | Sym BraceR <- t -> l : loop False ps ls
      | Sym Semi   <- t -> guardSemiClose l stack ps ls

    -- default case
    _ | Sym Semi <- t -> guardSemiClose l stack ps ls
      | otherwise     -> l : loop False stack ls

  loop _ stack (l : ls) = l : loop False stack ls

  -- catch the ;} case, and close with a normal BraceR.  additionally, close any
  -- open virtual blocks.
  guardSemiClose semi stack ps ts@(l @ Located { locThing = t } : ls)
    | Sym BraceR <- t =              l : loop False ps ls
    | otherwise       = closes ++ semi : loop False stack' ts
    where
    (stack',closes) = closeAllVirtual (locSource l) stack

  guardSemiClose _ _ _ [] = missingEof

  closeAllVirtual src stack = (stack',toks)
    where
    (nonExp,stack') = break (== Explicit) stack
    toks            = [ virt src VClose | block <- nonExp, isVirtual block ]

  -- closeLet will only close virtual blocks.  explicit blocks will cause a
  -- lexical error
  closeLet src = go []
    where
    go acc stack | Virtual{} : rest <- stack = go (virt src VClose:acc) rest
                 | Let       : rest <- stack = Just (acc,rest)
                 | otherwise                 = Nothing

  missingEof = panic "Mistral.Parser.Layout" [ "Missing Eof Token" ]

