-- vim: ft=haskell

{

-- alex-generated lexers have lots of warnings
{-# OPTIONS_GHC -w #-}

module Mistral.Parser.Lexer (
    primLexer
  ) where

import Mistral.Parser.LexerCore
import Mistral.Utils.Source

import           Numeric (readOct,readDec,readHex,readInt)
import           Data.Char (digitToInt)
import qualified Data.Text.Lazy as L

}

$cid_first = [A-Z]
$id_first  = [a-z_]
$id_next   = [a-zA-Z0-9_']
$id_dotlit = [a-zA-Z0-9_\.]
$id_colonlit = [a-zA-Z0-9_:]

$digit     = [0-9]
$hex_digit = [0-9a-fA-F]
$oct_digit = [0-7]
$bin_digit = [0-1]

@strPart   = [^\\\"]+

:-

<string> {
@strPart                { addString      }
\\n                     { litString "\n" }
\"                      { mkString       }
}

<0> {

-- skip whitespace
$white+                 { skip }

-- line comments
"--" $white .*          { skip }

"actions"               { emit $ KW KW_actions  }
"transitions"           { emit $ KW KW_transitions }

"topology"              { emit $ KW KW_topology }
"node"                  { emit $ KW KW_node     }
"taskSet"               { emit $ KW KW_taskSet  }
"if"                    { emit $ KW KW_if       }
"then"                  { emit $ KW KW_then     }
"else"                  { emit $ KW KW_else     }

"schedule"              { emit $ KW KW_schedule }
"using"                 { emit $ KW KW_using    }

"module"                { emit $ KW KW_module   }
"where"                 { emit $ KW KW_where    }
"let"                   { emit $ KW KW_let      }
"in"                    { emit $ KW KW_in       }
"data"                  { emit $ KW KW_data     }
"case"                  { emit $ KW KW_case     }
"of"                    { emit $ KW KW_of       }

"task"                  { emit $ KW KW_task     }
"after"                 { emit $ KW KW_after    }
"on"                    { emit $ KW KW_on       }
"start"                 { emit $ KW KW_start    }
"end"                   { emit $ KW KW_end      }
"timeout"               { emit $ KW KW_timeout  }
"at"                    { emit $ KW KW_at       }
"match"                 { emit $ KW KW_match    }
"tagged"                { emit $ KW KW_tagged   }

"import"                { emit $ KW KW_import }

"{"                     { emit $ Sym BraceL   }
"}"                     { emit $ Sym BraceR   }
";"                     { emit $ Sym Semi     }
"|"                     { emit $ Sym Pipe     }
"||"                    { emit $ Sym PipePipe }
"&&"                    { emit $ Sym AndAnd   }
"("                     { emit $ Sym ParenL   }
")"                     { emit $ Sym ParenR   }
"["                     { emit $ Sym BracketL }
"]"                     { emit $ Sym BracketR }
"="                     { emit $ Sym Assign   }
","                     { emit $ Sym Comma    }
"."                     { emit $ Sym Dot      }
":"                     { emit $ Sym Colon    }
"#"                     { emit $ Sym Hash     }
"->"                    { emit $ Sym ArrowR   }
"=>"                    { emit $ Sym FArrowR  }
"<-"                    { emit $ Sym ArrowL   }
"<->"                   { emit $ Sym ArrowBi  }
"::"                    { emit $ Sym DColon   }
"+"                     { emit $ Sym Plus     }
"-"                     { emit $ Sym Minus    }
"*"                     { emit $ Sym Mult     }
"/"                     { emit $ Sym Div      }
"!"                     { emit $ Sym Bang     }
"\"                     { emit $ Sym Lambda   }
"@"                     { emit $ Sym AtSym    }
"?"                     { emit $ Sym Question }
"_"                     { emit $ Sym Underscore }

\"                      { startString }


"0b" $bin_digit+        { number (readInt 2 (`elem` "01") digitToInt) 2 }
"0x" $hex_digit+        { number (readHex . drop 2) 16 }
"0o" $oct_digit+        { number (readOct . drop 2)  8 }
$digit+                 { number readDec            10 }

$id_first  $id_next*    { mkIdent  }
$cid_first $id_next*    { mkCIdent }
$id_colonlit+\.$id_colonlit+   { mkColonDotLit }
$id_colonlit+\/$id_colonlit+   { mkColonSlashLit }
$id_dotlit+\/$id_dotlit+       { mkDotSlashLit }
$id_dotlit+                    { mkDotLit }
$id_colonlit+                  { mkColonLit }

"#" $id_next $id_next* { mkAtom  }

}

{

stateToInt :: LexState -> Int
stateToInt Normal      = 0
stateToInt InString {} = string

primLexer :: Maybe FilePath -> L.Text -> [Lexeme]
primLexer mb bytes = loop (initialInput mb bytes) Normal
  where

  singleR ai a =
    Located { locSource = aiSource ai Range { rangeStart = aiPos ai
                                            , rangeEnd   = aiPos ai }
            , locThing  = a
            }

  loop ai sc = case alexScan ai (stateToInt sc) of

    AlexToken ai' len action ->
      let chunk    = L.take (fromIntegral len) (aiInput ai)
          range    = Range { rangeStart = aiPos ai
                           , rangeEnd   = L.foldl (flip moveChar) (aiPos ai) chunk
                           }
          (mb,sc') = action (aiSource ai range) chunk sc

          rest     = loop ai' sc'
       in maybe rest (:rest) mb


    AlexSkip ai' _ -> loop ai' sc

    AlexError ai' -> [singleR ai (Err LexicalError)]

    AlexEOF -> case sc of

      Normal -> [singleR ai Eof]

}
