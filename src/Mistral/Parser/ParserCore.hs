{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mistral.Parser.ParserCore where

import Mistral.ModuleSystem.Name
import Mistral.Parser.AST
import Mistral.Parser.Layout
import Mistral.Parser.Lexer
import Mistral.Parser.LexerCore
import Mistral.Utils.Panic
import Mistral.Utils.PP
import Mistral.Utils.Source

import           Control.Applicative (Applicative)
import           Data.List ( nub )
import           Data.Monoid (mconcat)
import qualified Data.Text.Lazy as L
import           MonadLib (runM,StateT,get,set,ExceptionT,raise,Id)


newtype Parser a = Parser
  { getParser :: StateT RW (ExceptionT ParseError Id) a
  } deriving (Functor,Monad, Applicative)

data RW = RW { rwInput  :: [Lexeme]
             , rwCursor :: Maybe Lexeme
             }

data ParseError = HappyError (Maybe Lexeme)
                | HappyErrorMsg Source String
                  deriving (Show)

instance HasSource ParseError where
  getSource pe = case pe of
    HappyError mb       -> getSource mb
    HappyErrorMsg src _ -> src

instance PP ParseError where
  ppr pe = case pe of
    HappyError mb -> case mb of
      Just l  -> text "Parse error near" <+> backquotes (pp (locThing l))
      Nothing -> text "Parse error"

    HappyErrorMsg _ str ->
      text "Parse error:" <+> text str

runParser :: Maybe FilePath -> L.Text -> Parser a -> Either ParseError a
runParser src txt p =
  case runM (getParser p) rw of
    Right (a,_) -> Right a
    Left e      -> Left e

  where
  rw = RW { rwInput  = layout (primLexer src txt)
          , rwCursor = Nothing
          }

lexerP :: (Lexeme -> Parser a) -> Parser a
lexerP k = Parser $ do
  rw <- get
  case rwInput rw of
    l : rest | Err e <- locThing l ->
               raise (HappyErrorMsg (locSource l) (descTokenError e))
             | otherwise ->
               do set RW { rwInput  = rest
                         , rwCursor = Just l
                         }
                  getParser (k l)
    []       -> raise (HappyError (rwCursor rw))



happyError :: Parser a
happyError  = Parser $ do
  rw <- get
  raise (HappyError (rwCursor rw))


-- Constructors ----------------------------------------------------------------

mkBind :: Located (Bind a) -> Decl a
mkBind lb = DSource (locSource lb) (DBind (locThing lb))

mkSig :: Located Signature -> Decl a
mkSig ls = DSource (locSource ls) (DSig (locThing ls))

mkTFuns :: [Type] -> Type
mkTFuns ts = case ts of
  []  -> panic "Mistral.Parser.ParserCore" [ "invalid function type" ]
  [t] -> t
  _   -> TSource (getSource ts) (foldr1 TFun ts)

mkSchema :: [Prop] -> Type -> Schema
mkSchema props ty = Forall params props ty (getSource ty)
  where
  params = [ TParam v | v <- nub (concatMap typeVars (ty:props)) ]

-- | Turn a type into a list of properties.  There's a good chance that these
-- will end up nonsensical, given how permissive the parsing is, but that should
-- all be resolved during type checking (or kind checking if we ever add that).
mkContext :: Type -> [Prop]
mkContext ty = case ty of
  TTuple cxt -> cxt
  _          -> [ty]

typeVars :: Type -> [Name]
typeVars ty = case ty of
  TApp f xs   -> typeVars f ++ concatMap typeVars xs
  TFun l r    -> typeVars l ++ typeVars r
  TVar n      -> [n]
  TCon _      -> []
  TTuple ts   -> concatMap typeVars ts
  TList t     -> typeVars t
  TSource _ t -> typeVars t

mkTFun :: Source -> Type -> Type -> Type
mkTFun src l r = TSource src (TFun l r)

mkTApp :: [Type] -> Type
mkTApp fs = case fs of
  [f]    -> f
  f : xs -> TSource (getSource fs) (TApp f xs)
  _      -> panic "Mistral.Parser.ParserCore" [ "invalid type application" ]

mkTTuple :: [Type] -> Type
mkTTuple ts = case ts of
  [t] -> t
  _   -> TTuple ts

mkEApp :: [Expr] -> Expr
mkEApp es = case es of
  [e]    -> e
  f : xs -> ESource (getSource es) (EApp f xs)
  _      -> panic "Mistral.Parser.ParserCore" [ "invalid application" ]

mkTuple :: [Expr] -> Expr
mkTuple es = case es of
  [e] -> e
  _   -> ETuple es

mkPTuple :: [Pattern] -> Pattern
mkPTuple ps = case ps of
  [p] -> p
  _   -> PTuple ps

mkPat :: Expr -> Parser Pattern
mkPat e = case e of
  ETuple es        -> PTuple `fmap` mapM mkPat es
  ESource src e'   -> PSource src `fmap` mkPat e'

  ELit lit         -> return (PLit lit)
  EVar n           -> return (PVar n)
  ECon n           -> return (PCon n [])
  EApp (ECon c) es -> PCon c `fmap` mapM mkPat es
  EApp (ESource s (ECon c)) es
                   -> (PSource s . PCon c) `fmap` mapM mkPat es

  EWildPat         -> return PWildcard

  x -> fail $ "Invalid pattern: " ++ show x

mkLink :: LinkTarget -> LinkType -> LinkTarget -> [Located Tag]
       -> Located Link
mkLink a ty b tags =
    Link ty a b tags
        `at` mconcat [getSource a, getSource b, getSource tags]

mkTime :: String -> Time
mkTime str =
    let (hh, str2) = break (==':') str
        (mm, str3) = break (==':') $ drop 1 str2
        (ss, dd  ) = break (=='.') $ drop 1 str3
        hhI        = readMb hh
        mmI        = readMb mm
        ssI        = readMb ss
        ddF        = readMb ("0" ++ dd) :: Double
        ddNS       = floor $ ddF * fromIntegral nsInSec
        nsInSec    = 10^(9 :: Int) :: Int
    in Time hhI mmI ssI ddNS

readMb :: (Read a, Num a) => String -> a
readMb s =
    case readsPrec 0 s of
        ((r,_):_) -> r
        _         -> 0
