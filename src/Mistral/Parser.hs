module Mistral.Parser (
    parseFile
  , parseModule

  , testLexer
  , testLayout
  ) where

import           Mistral.Driver
import qualified Mistral.Parser.AST        as P
import qualified Mistral.Parser.Finalize   as P
import qualified Mistral.Parser.Layout     as P
import qualified Mistral.Parser.Lexer      as P
import qualified Mistral.Parser.LexerCore  as P
import qualified Mistral.Parser.Parser     as P
import qualified Mistral.Parser.ParserCore as P
import qualified Mistral.Parser.Unlit      as P
import           Mistral.Utils.PP

import           Control.Monad ( mzero )
import           Data.Char ( toLower )
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L
import           System.FilePath ( takeExtension )


testLexer :: String -> [P.Lexeme]
testLexer  = P.primLexer Nothing . L.pack

testLayout :: String -> [P.Lexeme]
testLayout  = P.layout . testLexer

parseModule :: Maybe FilePath -> L.Text -> Driver P.Module
parseModule src bytes = phase "parse" (parseModule' src bytes)


parseFile :: FilePath -> Driver P.Module
parseFile path = phase "parse" $ do

  unlit <- case map toLower (takeExtension path) of
             ".md"       -> return (P.unlit . P.markdown)
             ".markdown" -> return (P.unlit . P.markdown)
             ".mst"      -> return id
             _           -> do addErr (unknownExtension path)
                               mzero

  bytes <- io (L.readFile path)
  parseModule' (Just path) (unlit bytes)

unknownExtension :: FilePath -> PPDoc
unknownExtension path = text "Unknown file extension:" <+> text path

parseModule' :: Maybe FilePath -> L.Text -> Driver P.Module
parseModule' src bytes =
  do m <- case P.runParser src bytes P.parseModule of
            Right m -> P.finalize m
            Left pe -> do addErrLoc pe
                          mzero

     traceMsg (text "" $$ pp m)

     return m

