{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mistral.CodeGen.Specialize ( specialize ) where

import Mistral.Driver
import Mistral.ModuleSystem.Name
import Mistral.TypeCheck.AST
import Mistral.TypeCheck.Unify
import Mistral.Utils.PP
import Mistral.Utils.SCC ( Group(..) )

import           Control.Applicative ( Applicative(..), (<$>), Alternative )
import           Control.Monad ( MonadPlus(..), when )
import qualified Data.Foldable as F
import           Data.Maybe ( isNothing )
import           Data.Traversable ( traverse )
import           MonadLib ( runM, BaseM(..), StateT, get, set, WriterT, put)
import qualified Data.Map as Map


-- | Specialize a module.  This currently assumes that the module has already
-- been lambda-lifted, and will emit any specializations to the top-level,
-- instead of to the block they are defined in.
specialize :: Program -> Driver Program
specialize p = phase "spec" $ failErrs $
  do (p',insts) <- runSpec (progDeclEnv p) (specProgram p)

     let spec_p = p' { progBinds = progBinds p' ++ insts }
     traceMsg (text "" $$ pp spec_p)

     return spec_p


-- Specialization Monad --------------------------------------------------------

data RW = RW { rwFresh :: !Int
               -- ^ Fresh names for instances
             , rwInsts :: Map.Map (Name,[Type]) Name
               -- ^ Specializations
             , rwDecls :: DeclEnv
               -- ^ All known functions
             }

emptyRW :: DeclEnv -> RW
emptyRW env = RW { rwFresh = 0
                 , rwInsts = Map.empty
                 , rwDecls = env }

newtype Spec a = SpecM { unSpec :: StateT RW (WriterT [Group Decl] Driver) a
                       } deriving (Functor,Applicative,Monad,MonadPlus,Alternative)

runSpec :: DeclEnv -> Spec a -> Driver (a,[Group Decl])
runSpec env m =
  do ((a,_),insts) <- runM (unSpec m) (emptyRW env)
     return (a, insts)

instance BaseM Spec Driver where
  inBase m = SpecM (inBase m)


freshName :: Name -> Spec Name
freshName n = SpecM $
  do rw <- get
     set $! rw { rwFresh = rwFresh rw + 1 }
     return (mkFresh (Spec n) (rwFresh rw))

lookupDecl :: Name -> Spec (Maybe (Group Decl))
lookupDecl n = SpecM $
  do rw <- get
     let mb = Map.lookup n (rwDecls rw)
     when (isNothing mb) (addErr (text "Unable to find the body of" <+> pp n))
     return mb

lookupSpec :: Name -> [Type] -> Spec (Maybe Name)
lookupSpec n tys = SpecM $
  do rw <- get
     return (Map.lookup (n,tys) (rwInsts rw))

-- | Register a specialization.
addSpec :: Name -> [Type] -> Spec Name
addSpec n tys =
  do specName <- freshName n
     SpecM $ do rw <- get
                set rw { rwInsts = Map.insert (n,tys) specName (rwInsts rw) }
     return specName

addInst :: Group Decl -> Spec ()
addInst g = SpecM (put [g])


-- Module Declaration Environment ----------------------------------------------

-- | The boolean indicates if the element of the group should be kept in the
-- final program.
type DeclEnv = Map.Map Name (Group Decl)

-- | This relies on lambda lifting having been run before specialization.
progDeclEnv :: Program -> DeclEnv
progDeclEnv m = F.foldMap declGroupEnv (progBinds m)

declGroupEnv :: Group Decl -> DeclEnv
declGroupEnv g = F.foldl declEnv Map.empty g
  where
  declEnv ds d = Map.insert (bName d) g ds


-- Function Specialization -----------------------------------------------------

-- | Given a declaration name, and a list of types to apply, return the name of
-- the specialized instance.
inst :: Name -> [Type] -> Spec Expr
inst n tys
  | any isPoly tys =
    do traceMsg (text "ignoring polymorphic call:" <+> pp orig)
       return orig
  | otherwise      =
    do mb <- lookupSpec n tys
       traceMsg (text "specializing:" <+> pp orig)
       case mb of
         Just n' -> return (EVar n')
         Nothing ->
           do mbG <- lookupDecl n
              case mbG of
                Just g  -> do instGroup tys g
                              maybe orig EVar `fmap` lookupSpec n tys
                Nothing -> return orig
  where
  orig = tappE (EVar n) tys

-- | Specialize a group, emitting specialized versions of all functions
-- involved.
instGroup :: [Type] -> Group Decl -> Spec ()
instGroup tys g = addInst =<< traverse (instDecl tys) =<< traverse step g
  where
  step d = do n' <- addSpec (bName d) tys
              return d { bName = n' }

instDecl :: [Type] -> Decl -> Spec Decl
instDecl tys d =
  do let subst       = boundBinds (zip (bTParams d) tys)
         instField p = apply subst (p d)
         d' = d { bTParams = []
                , bCParams = instField bCParams
                , bParams  = instField bParams
                , bBody    = instField bBody
                , bResult  = instField bResult }
     body' <- specExpr (bBody d')
     return d' { bBody = body' }


-- Specialization Traversal ----------------------------------------------------

-- | Specialize a module.
specProgram :: Program -> Spec Program
specProgram p =
  do binds' <- traverse specGroup (progBinds p)
     return p { progBinds = binds' }

specExpr :: Expr -> Spec Expr
specExpr e = case e of

  ETApp (EVar n) ts -> inst n ts
  EApp f x      -> EApp <$> specExpr f            <*> specExpr x
  ELet gs e' ty -> ELet <$> traverse specGroup gs <*> specExpr e' <*> pure ty

  ECase src cases rty -> ECase src <$> specMatch cases <*> pure rty

  EStmts ity ty as  -> EStmts ity ty <$> specActions as
  ETApp e' ts    -> ETApp <$> specExpr e' <*> pure ts
  ECApp e' es    -> ECApp <$> specExpr e' <*> traverse specExpr es
  EMkTuple elems -> let (tys,es) = unzip elems
                     in EMkTuple . zip tys <$> traverse specExpr es
  EMkList ty es  -> EMkList ty <$> traverse specExpr es

  EPrim _     -> pure e
  EVar _      -> pure e
  ECon _      -> pure e
  ELit _      -> pure e
  ETopo _     -> pure e
  ETaskSet _  -> pure e
  ESchedule _ -> pure e

specGroup :: Group Decl -> Spec (Group Decl)
specGroup  = traverse specDecl

specDecl :: Decl -> Spec Decl
specDecl d =
  do body' <- specExpr (bBody d)
     return d { bBody = body' }

specMatch :: Match pat -> Spec (Match pat)
specMatch m = case m of
  MCase s sty m'    -> MCase    <$> specExpr s  <*> pure sty <*> specMatch m'
  MRename n e ty m' -> MRename n <$> specExpr e <*> pure ty <*> specMatch m'
  MPat pat m'       -> MPat pat <$> specMatch m'
  MGuard g m'       -> MGuard   <$> specExpr g <*> specMatch m'
  MSplit l r        -> MSplit   <$> specMatch l <*> specMatch r
  MFail             -> pure m
  MExpr e           -> MExpr    <$> specExpr e

specActions :: [Action] -> Spec [Action]
specActions  = mapM $ \ a -> case a of

  ABind mb e ty ->
    do e' <- specExpr e
       return (ABind mb e' ty)

  AReceive r fs to w -> AReceive r <$> traverse specFrom fs
                                   <*> traverse specTimeout to
                                   <*> traverse specExpr w

specFrom :: From -> Spec From
specFrom (From src msg msgTy m) = From src msg msgTy <$> specMatch m

specTimeout :: Timeout -> Spec Timeout
specTimeout (Timeout to body) = Timeout <$> specExpr to <*> specExpr body
