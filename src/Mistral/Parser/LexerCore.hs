{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mistral.Parser.LexerCore where

import Mistral.Utils.Source
import Mistral.Utils.Panic ( panic )
import Mistral.Utils.PP

import Data.Bits (shiftR,(.&.))
import Data.Char (ord)
import Data.Word (Word8)
import qualified Data.Text.Lazy as L


-- Lexer Monad -----------------------------------------------------------------

-- | Lexer state.
data AlexInput = AlexInput { aiPos    :: !Position
                           , aiSource :: Range -> Source
                           , aiChar   :: !Char
                           , aiBytes  :: [Word8]
                           , aiInput  :: L.Text
                           }

initialInput :: Maybe FilePath -> L.Text -> AlexInput
initialInput mb bytes =
  AlexInput { aiPos    = Position 1 1 0
            , aiSource = maybe NoSource Source mb
            , aiChar   = '\n'
            , aiBytes  = []
            , aiInput  = bytes
            }

-- | Decode a single character, and dump its octets into the byte buffer.
fillBuffer :: AlexInput -> Maybe AlexInput
fillBuffer ai = do
  (c,rest) <- L.uncons (aiInput ai)
  return ai { aiPos   = moveChar c (aiPos ai)
            , aiBytes = utf8Encode c
            , aiChar  = c
            , aiInput = rest
            }

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 +  oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + ( oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 +   oc             .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + ( oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6)  .&. 0x3f)
                        , 0x80 +   oc              .&. 0x3f
                        ]


-- Alex Compatibility ----------------------------------------------------------

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar  = aiChar

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte ai = case aiBytes ai of
  b : rest -> return (b, ai { aiBytes = rest })
  _        -> alexGetByte =<< fillBuffer ai


-- Lexer Actions ---------------------------------------------------------------

data LexState = Normal
              | InString String

type Action = Source -> L.Text -> LexState -> (Maybe Lexeme, LexState)

-- | Emit a token.
emit :: Token -> Action
emit tok src _chunk sc = (Just (tok `at` src), sc)

-- | Skip the current input.
skip :: Action
skip _ _ sc = (Nothing,sc)

number :: ReadS Integer -> Int -> Action
number parse base src chunk sc = (Just (tok `at` src), sc)
  where
  tok = case parse (L.unpack chunk) of
          [ (i,_) ] -> Num base i
          _         -> Err LexicalError

-- | Generate an identifier token.
mkIdent :: Action
mkIdent src chunk sc = (Just (Ident (L.unpack chunk) `at` src), sc)

mkOperator :: Action
mkOperator src chunk sc = (Just (Operator (L.unpack chunk) `at` src), sc)

mkCIdent :: Action
mkCIdent src chunk sc = (Just (CIdent (L.unpack chunk) `at` src), sc)

-- | Generate an atom token.
mkAtom :: Action
mkAtom src chunk sc = (Just (Atom (tail (L.unpack chunk)) `at` src), sc)

-- | Generate a dot literal (ex: IP address)
mkDotLit :: Action
mkDotLit src chunk sc = (Just (DotT (L.unpack chunk) `at` src), sc)

-- | Generate a colon+dot literal (ex: time with fractional seconds)
mkColonDotLit :: Action
mkColonDotLit src chunk sc = (Just (ColonDotT (L.unpack chunk) `at` src), sc)

-- | Generate a colon+slash literal (ex: IPv6 netmask)
mkColonSlashLit :: Action
mkColonSlashLit src chunk sc = (Just (ColonSlashT (L.unpack chunk) `at` src), sc)

-- | Generate a dot+slash literal (ex: IPv4 netmask)
mkDotSlashLit :: Action
mkDotSlashLit src chunk sc = (Just (DotSlashT (L.unpack chunk) `at` src), sc)

-- | Generate an atom token.
mkColonLit :: Action
mkColonLit src chunk sc = (Just (ColonT (L.unpack chunk) `at` src), sc)

-- String Literals -------------------------------------------------------------

startString :: Action
startString _ _ _ = (Nothing, InString "")

-- | Discard the chunk, and use this string instead.
litString :: String -> Action
litString lit _ _ (InString str) = (Nothing, InString (str ++ lit))
litString _   _ _ _              = panic "Mistral.Parser.Lexer"
                                       [ "Expected string literal state" ]

addString :: Action
addString _ chunk (InString str) = (Nothing, InString (str ++ L.unpack chunk))
addString _ _     _              = panic "Mistral.Parser.Lexer"
                                       [ "Expected string literal state" ]

mkString :: Action
mkString src _ (InString str) = (Just (String str `at` src), Normal)
mkString _   _ _              = panic "Mistral.Parser.Lexer"
                                       [ "Expected string literal state" ]


-- Tokens ----------------------------------------------------------------------

type Lexeme = Located Token

data Token = KW Keyword
           | Num Int Integer -- ^ Base and value
           | ColonDotT String
           | ColonSlashT String
           | DotSlashT String
           | DotT String
           | ColonT String
           | String String
           | Ident String
           | Operator String
           | CIdent String
           | Atom String
           | Sym Symbol
           | Virt Virt
           | Eof
           | Err TokenError
             deriving (Show,Eq)

-- | Virtual symbols inserted for layout purposes.
data Virt = VOpen
          | VClose
          | VSep
            deriving (Show,Eq)

data Keyword = KW_topology
             | KW_node
             | KW_taskSet
             | KW_precept
             | KW_if
             | KW_then
             | KW_else
             | KW_module
             | KW_where
             | KW_let
             | KW_in

             | KW_import

             | KW_schedule
             | KW_using
             | KW_actions
             | KW_transitions

             | KW_task
             | KW_on
             | KW_after
             | KW_start
             | KW_end
             | KW_timeout
             | KW_at
             | KW_match
             | KW_tagged
             | KW_data
             | KW_case
             | KW_of
               deriving (Show,Eq)

data Symbol = BracketL
            | BracketR
            | Semi
            | SemiClose
            | Pipe
            | PipePipe
            | AndAnd
            | BraceL
            | BraceR
            | ParenL
            | ParenR
            | Assign
            | Comma
            | Dot
            | Colon
            | Hash
            | FArrowR
            | ArrowR
            | ArrowL
            | ArrowBi
            | DColon
            | Plus
            | Minus
            | Mult
            | Div
            | Bang
            | Lambda
            | AtSym
            | Question
            | Underscore
              deriving (Show,Eq)

data TokenError = LexicalError
                  deriving (Show,Eq)

descTokenError :: TokenError -> String
descTokenError e = case e of
  LexicalError -> "lexical error"


instance PP Token where
  ppr t = case t of
    KW kw         -> pp kw
    Num _ v       -> integer v
    ColonDotT s   -> text s
    ColonSlashT s -> text s
    DotSlashT s   -> text s
    DotT s        -> text s
    ColonT s      -> text s
    String s      -> text s
    Ident s       -> text s
    Operator s    -> parens (text s)
    CIdent s      -> text s
    Atom s        -> char '#' <> text s
    Sym sym       -> pp sym
    Virt v        -> pp v
    Eof           -> text "EOF"
    Err e         -> pp e

instance PP Virt where
  ppr v = case v of
    VOpen  -> text "v{"
    VClose -> text "v}"
    VSep   -> text "v;"


instance PP Keyword where
  ppr kw = case kw of
    KW_topology     -> text "topology"
    KW_node         -> text "node"
    KW_taskSet      -> text "taskSet"
    KW_precept      -> text "precept"
    KW_if           -> text "if"
    KW_then         -> text "then"
    KW_else         -> text "else"
    KW_module       -> text "module"
    KW_where        -> text "where"
    KW_let          -> text "let"
    KW_in           -> text "in"
    KW_schedule     -> text "schedule"
    KW_using        -> text "using"
    KW_actions      -> text "actions"
    KW_transitions  -> text "transitions"
    KW_task         -> text "task"
    KW_on           -> text "on"
    KW_after        -> text "after"
    KW_start        -> text "start"
    KW_end          -> text "end"
    KW_timeout      -> text "timeout"
    KW_at           -> text "at"
    KW_import       -> text "import"
    KW_match        -> text "match"
    KW_tagged       -> text "tagged"
    KW_data         -> text "data"
    KW_case         -> text "case"
    KW_of           -> text "of"

instance PP Symbol where
  ppr sym = case sym of
    BracketL     -> char '['
    BracketR     -> char ']'
    Semi         -> char ';'
    SemiClose    -> text ";}"
    BraceL       -> char '{'
    BraceR       -> char '}'
    ParenL       -> char '('
    ParenR       -> char ')'
    Assign       -> char '='
    Comma        -> char ','
    Dot          -> char '.'
    Colon        -> char ':'
    Hash         -> char '#'
    FArrowR      -> text "=>"
    ArrowR       -> text "->"
    ArrowL       -> text "<-"
    ArrowBi      -> text "<->"
    DColon       -> text "::"
    Plus         -> char '+'
    Minus        -> char '-'
    Mult         -> char '*'
    Div          -> char '/'
    Bang         -> char '!'
    Lambda       -> char '\\'
    AtSym        -> char '@'
    Question     -> char '?'
    Underscore   -> char '_'
    Pipe         -> char '|'
    PipePipe     -> text "||"
    AndAnd       -> text "&&"

instance PP TokenError where
  ppr _ = text "lexical error"
