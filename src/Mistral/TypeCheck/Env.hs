module Mistral.TypeCheck.Env (
    Env(..)
  , VarType(..), vtSchema, vtExpr
  , lookupEnv, addSchema, checkingSchema, primitiveSchema
  , lookupTSyn, addTSyn
  , depModules
  , showEnv
  , getInsts
  ) where

import qualified Mistral.ModuleSystem.Interface as Iface
import           Mistral.ModuleSystem.Name ( Name )
import           Mistral.TypeCheck.AST
                     ( Schema, TySyn(..), Expr(EPrim), Prim, Inst )
import           Mistral.TypeCheck.Unify ( Types(..) )
import           Mistral.Utils.PP

import           Control.Applicative ( (<|>) )
import           Data.Foldable ( foldMap )
import qualified Data.Map as Map
import           Data.Monoid ( Monoid(..) )

data VarType = Var Schema
             | Checking Schema Expr
             | Primitive Schema Prim
               deriving (Show)

instance Types VarType where
  typeVars vt = case vt of
    Var s         -> typeVars s

    -- be careful to never force the expression
    Checking s _  -> typeVars s
    Primitive s _ -> typeVars s

  applyBndrs b u vt = case vt of
    Var s         -> Var (applyBndrs b u s)

    -- be careful to never force the expression
    Checking s e  -> Checking (applyBndrs b u s) e
    Primitive s p -> Primitive (applyBndrs b u s) p

depModules :: Env -> [Iface.Iface]
depModules env = Iface.getIfaces (envIfaces env)

vtSchema :: VarType -> Schema
vtSchema vt = case vt of
  Var s         -> s
  Checking s _  -> s
  Primitive s _ -> s

vtExpr :: VarType -> Maybe Expr
vtExpr vt = case vt of
  Var _         -> Nothing
  Checking _ e  -> Just e
  Primitive _ e -> Just (EPrim e)

data Env = Env { envTypes  :: Map.Map Name VarType
               , envTSyns  :: Map.Map Name TySyn
               , envIfaces :: Iface.IfaceTrie
               } deriving (Show)

getInsts :: Env -> [Inst]
getInsts env = Iface.getInsts (envIfaces env)

-- | A version of show that doesn't touch the recursive parts of the 'VarType'.
showEnv :: Env -> String
showEnv env = pretty $
  vcat [ text "Types"
       , vcat [ pp n <+> char '=' <+> pp (vtSchema vt)
              | (n,vt) <- Map.toList (envTypes env) ]
       , text "Syns"
       , vcat [ pp syn | syn <- Map.elems (envTSyns env) ]
       ]

instance Monoid Env where
  mempty        = Env { envTypes  = Map.empty
                      , envTSyns  = Map.empty
                      , envIfaces = mempty }
  mappend l r   = Env { envTypes  = merge envTypes
                      , envTSyns  = merge envTSyns
                      , envIfaces = mappend (envIfaces l) (envIfaces r) }
    where
    merge p = Map.union (p l) (p r)
  mconcat es    = Env { envTypes  = merge envTypes
                      , envTSyns  = merge envTSyns
                      , envIfaces = foldMap envIfaces es }
    where
    merge p = Map.unions (map p es)

instance Types Env where
  typeVars  env      = typeVars  (Map.elems (envTypes env))
  applyBndrs i u env = env { envTypes = applyBndrs i u `fmap` envTypes env }

-- | Lookup a schema in the environment.  First check the local environment,
-- then fall back on looking up in the interface environment.
lookupEnv :: Name -> Env -> Maybe VarType
lookupEnv qn env = Map.lookup qn (envTypes env) <|> fromIface
  where
  fromIface =
    do bind <- Iface.lookupBind qn (envIfaces env)
       case bind of
         Iface.IfaceBind ty   -> return (Var ty)
         Iface.IfacePrim ty e -> return (Primitive ty e)

-- | Record a name/schema association in the environment.
addSchema :: Name -> Schema -> Env -> Env
addSchema qn ty env =
  env { envTypes = Map.insert qn (Var ty) (envTypes env) }

-- | A type that's currently being checked.
checkingSchema :: Name -> Schema -> Expr -> Env -> Env
checkingSchema qn ty ex env =
  env { envTypes = Map.insert qn (Checking ty ex) (envTypes env) }

-- | A primitive, with its expression rewriting.
primitiveSchema :: Name -> Schema -> Prim -> Env -> Env
primitiveSchema qn ty ex env =
  env { envTypes = Map.insert qn (Primitive ty ex) (envTypes env) }

-- | Record a name/type synonym association in the environment.
addTSyn :: TySyn -> Env -> Env
addTSyn syn env = env { envTSyns = Map.insert (synName syn) syn (envTSyns env) }

-- | Lookup a type synonym in the environment.  Check the local environment,
-- then fall back on the interface environment.
lookupTSyn :: Name -> Env -> Maybe TySyn
lookupTSyn n env = Map.lookup n (envTSyns env)
               <|> Iface.lookupTySyn n (envIfaces env)
