{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Mistral.Utils.PP where

import Mistral.ModuleSystem.Name
import Mistral.Utils.Source

import           Control.Applicative ( Applicative(..), (<$>) )
import           MonadLib ( Id, ReaderT, runM, ask, local )
import qualified Text.PrettyPrint.HughesPJ as P


-- Utilities -------------------------------------------------------------------

backtick :: PPDoc
backtick  = char '`'

backquotes :: PPDoc -> PPDoc
backquotes d = hcat [ backtick, d, backtick ]

vcatSpaced :: [PPDoc] -> PPDoc
vcatSpaced  = foldl step empty
  where
  step acc d = acc $$ text "" $$ d

block :: [PPDoc] -> PPDoc
block stmts =
  do layout <- useLayout
     if layout
        then vcat stmts
        else list (char '{') (char ';') (char '}') stmts


-- Pretty Printer --------------------------------------------------------------

pretty :: PP a => a -> String
pretty  = P.render . runPPM defaultPPEnv . pp

data PPEnv = PPEnv { pePrec      :: !Int
                   , peLayout    :: Bool
                   , peRealNames :: Bool
                   }

defaultPPEnv :: PPEnv
defaultPPEnv  = PPEnv { pePrec      = 0
                      , peLayout    = False
                      , peRealNames = False
                      }


newtype PPM a = PPM { getPPM :: ReaderT PPEnv Id a
                    } deriving (Functor,Applicative,Monad)

runPPM :: PPEnv -> PPM a -> a
runPPM env m = runM (getPPM m) env


type PPDoc = PPM P.Doc

instance Show PPDoc where
  show = P.render . runPPM defaultPPEnv


useLayout :: PPM Bool
useLayout  = PPM (peLayout `fmap` ask)

dispRealNames :: PPM Bool
dispRealNames  = PPM (peRealNames `fmap` ask)

currentPrec :: PPM Int
currentPrec  = PPM (pePrec `fmap` ask)

pp :: PP a => a -> PPDoc
pp  = ppPrec 0

-- | Pretty-print at a specific precedence level.
ppPrec :: PP a => Int -> a -> PPDoc
ppPrec p a = PPM $
  do pe <- ask
     local (pe { pePrec = p }) (getPPM (ppr a))


class PP a where
  ppr :: a -> PPDoc

instance PP P.Doc where
  ppr = return

instance PP PPDoc where
  ppr = id

instance PP Int where
  ppr = int

instance PP Position where
  ppr p = pp (posRow p) <> char ',' <> pp (posCol p)

instance PP Range where
  ppr r = pp (rangeStart r) <> char '-' <> pp (rangeEnd r)

instance PP Source where
  ppr s = case s of
    Unknown    -> empty
    NoSource r -> pp r
    Source f r -> text f <> char ':' <> pp r

-- By default, strip out location information.
instance PP a => PP (Located a) where
  ppr l = ppr (locThing l)

ppLoc :: PP a => Located a -> PPDoc
ppLoc l = parens (commas [ppr (locSource l), ppr (locThing l)])

instance PP Name where
  ppr n =
    do useReal <- dispRealNames
       if useReal
          then ppr (nReal n)
          else case n of
                 Parsed s _   -> text s
                 Generated rn -> ppr rn

instance PP RealName where
  ppr rn = case rn of
    Local str     -> text str
    Global ns str -> hcat (punctuate (char '.') (map text (ns ++ [str])))
    Fresh pass i  -> braces (text (show pass) <> char ':' <> int i)


-- Wrapped Functions -----------------------------------------------------------

infixl 6 <>
(<>) :: PPDoc -> PPDoc -> PPDoc
l <> r = (P.<>) <$> l <*> r

infixl 6 <+>
(<+>) :: PPDoc -> PPDoc -> PPDoc
l <+> r = (P.<+>) <$> l <*> r

infixl 5 $$
($$) :: PPDoc -> PPDoc -> PPDoc
l $$ r = (P.$$) <$> l <*> r

commas :: [PPDoc] -> PPDoc
commas ds = (P.sep . P.punctuate (P.char ',')) `fmap` sequence ds

parens :: PPDoc -> PPDoc
parens  = fmap P.parens

precParens :: Int -> PPDoc -> PPDoc
precParens p body =
  do c <- currentPrec
     if c >= p
        then P.parens `fmap` body
        else                 body

text :: String -> PPDoc
text str = return (P.text str)

char :: Char -> PPDoc
char c = return (P.char c)

int :: Int -> PPDoc
int i = return (P.int i)

integer :: Integer -> PPDoc
integer i = return (P.integer i)

vcat :: [PPDoc] -> PPDoc
vcat ds = P.vcat `fmap` sequence ds

hcat :: [PPDoc] -> PPDoc
hcat ds = P.hcat `fmap` sequence ds

hsep :: [PPDoc] -> PPDoc
hsep ds = P.hsep `fmap` sequence ds

fsep :: [PPDoc] -> PPDoc
fsep ds = P.fsep `fmap` sequence ds

sep :: [PPDoc] -> PPDoc
sep ds = P.sep `fmap` sequence ds

punctuate :: PPDoc -> [PPDoc] -> [PPDoc]
punctuate _ []     = []
punctuate p (x:xs) = go x xs
  where
  go y (d:ds) = y <> p : go d ds
  go y []     = [y]

braces :: PPDoc -> PPDoc
braces  = fmap P.braces

brackets :: PPDoc -> PPDoc
brackets  = fmap P.brackets

doubleQuotes :: PPDoc -> PPDoc
doubleQuotes  = fmap P.doubleQuotes

empty :: PPDoc
empty  = return P.empty

hang :: PPDoc -> Int -> PPDoc -> PPDoc
hang d1 n d2 = P.hang <$> d1 <*> return n <*> d2

nest :: Int -> PPDoc -> PPDoc
nest i = fmap (P.nest i)

list :: PPDoc -> PPDoc -> PPDoc -> [PPDoc] -> PPDoc
list open _ close []     = open <> close
list open p close (d:ds) = vcat (open <+> d : go ds)
  where
  go (x:xs) = p <+> x : go xs
  go []     = [close]
