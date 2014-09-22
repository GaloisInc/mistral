{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mistral.CodeGen.LambdaLift (
    lambdaLift
  ) where

import Mistral.Driver
import Mistral.ModuleSystem.Export ( Export(..) )
import Mistral.TypeCheck.AST
import Mistral.TypeCheck.Unify
import Mistral.Utils.Names
import Mistral.Utils.PP
import Mistral.Utils.SCC ( Group(..), groupElems, scc )

import           Control.Applicative ( Applicative(..), (<$>), Alternative )
import           Control.Monad ( MonadPlus )
import qualified Data.Foldable as F
import qualified Data.Map as Map
import           Data.Maybe ( mapMaybe, catMaybes )
import           Data.Monoid ( mappend, mconcat )
import qualified Data.Set as Set
import qualified Data.Traversable as T
import           MonadLib ( runM, BaseM(..), WriterT, put, ReaderT, ask, local )


-- | Perform lambda-lifting on a module.
lambdaLift :: Module -> Driver Module
lambdaLift m = phase "ll" $
  do (m',lifted) <- runLL (llModule m)

     let binds = concatMap groupElems (modBinds m')
              ++ concatMap groupElems lifted

     let ll = m' { modBinds = scc binds }

     traceMsg (text "" $$ pp ll)

     return ll


-- Lambda-lifting Monad --------------------------------------------------------

type Closure = Map.Map Name Type

paramClosure :: [Param] -> Closure
paramClosure ps = Map.fromList [ (n,ty) | Param n ty <- ps ]

patClosure :: Pattern -> Closure
patClosure pat = case pat of
  PCon _ ps -> paramClosure (catMaybes ps)
  PTuple ps -> paramClosure (catMaybes ps)
  PLit _    -> Map.empty

ppClosure :: Closure -> PPDoc
ppClosure clos =
  commas [ pp x <> char ':' <> pp ty | (x,ty) <- Map.toList clos ]

closTParams :: Closure -> [TParam]
closTParams clos = Set.toList (typeVars (Map.elems clos))

closParams :: Closure -> [Param]
closParams clos = [ Param n ty | (n,ty) <- Map.toList clos ]

-- | Call-site rewriting.
type Rewrite = Map.Map Name Closure

rewrite :: Name -> Closure -> Rewrite
rewrite  = Map.singleton

data Env = Env { envDepth :: !Int
                 -- ^ Number of type parameters bound
               , envClosure :: Closure
                 -- ^ The current largest closure
               , envParams :: Closure
                 -- ^ Names provided as paramters
               , envRewrite :: Rewrite
                 -- ^ Rewriting call sites
               }

emptyEnv :: Env
emptyEnv  = Env { envDepth   = 0
                , envClosure = Map.empty
                , envParams  = Map.empty
                , envRewrite = Map.empty }


newtype LL a = LL { unLL :: ReaderT Env (WriterT [Group Decl] Driver) a
                  } deriving (Functor,Applicative,Monad,MonadPlus,Alternative)

runLL :: LL a -> Driver (a,[Group Decl])
runLL m = runM (unLL m) emptyEnv

instance BaseM LL Driver where
  {-# INLINE inBase #-}
  inBase m = LL (inBase m)

-- | Emit a group of declarations to the top level.  It's assumed that these
-- have already had their closures and call-sites adjusted.
emitGroup :: Group Decl -> LL ()
emitGroup g = LL (put [fmap mkTopLevel g])
  where
  mkTopLevel b = b { bExport = Private }

-- | Merge some rewrites into the environment.
addRewrite :: Rewrite -> LL a -> LL a
addRewrite rw m = LL $
  do env <- ask
     local env { envRewrite = rw `Map.union` envRewrite env } (unLL m)

-- | Rewrite the use of a name, adding any additional closure arguments
-- required.
rewriteCall :: Name -> [Type] -> LL Expr
rewriteCall n ts = LL $
  do env <- ask
     case Map.lookup n (envRewrite env) of
       Nothing   -> return (tappE (EVar n) ts)
       Just clos ->
         do let tys = [ TVar (TVFree p) | p <- closTParams clos ]
                cps = [ EVar (pName p)  | p <- closParams  clos ]
                new = appC (tappE (EVar n) (ts ++ tys)) cps
            traceMsg (pp (tappE (EVar n) ts) <+> text "~>" <+> pp new)
            return new

-- | Given the free variables in a structure, determine which variables in the
-- current closure it will require.
getClosure :: FreeVars a => a -> LL Closure
getClosure a = LL $
  do env <- ask
     let fvs = freeVars a
         -- variables captured in the closure
         fromEnv = Map.filterWithKey (\k _ -> k `Set.member` fvs)
                                     (envClosure env)
         -- variables added by the closure of others
         fromRewrites = mapMaybe step (Set.toList fvs)
           where
           step n = Map.lookup n (envRewrite env)

     return (Map.unions (fromEnv : fromRewrites))


-- | Extend the current closure.
extendClosure :: Closure -> LL a -> LL a
extendClosure clos m = LL $
  do env <- ask
     local env { envClosure = clos `Map.union` envClosure env } (unLL m)

-- | Run an action with the binding environment produced by a binding.
--
-- XXX this doesn't apply the substitution to the body of the binding at all, so
-- if we ever implement scoped-type-variables, we'll have to switch to passing a
-- substituted body in the continuation as well.
withParams :: Bind a -> (Subst -> LL b) -> LL b
withParams b body = LL $
  do env <- ask
     let -- introduce variables for type parameters...
         var i p   = (p, TVar (TVFree (p { tpIndex = i })))
         to        = boundBinds (zipWith var [envDepth env ..] (bTParams b))

         -- ... and remove them later
         bind i p  = (p { tpIndex = i }, TVar (TVBound p))
         from      = freeBinds (zipWith bind [envDepth env ..] (bTParams b))

         -- instantiate the arguments
         toParam p = (pName p, pType p)
         params    = Map.fromList (map toParam (apply to (bParams b)))

     local env { envDepth   = envDepth env + length (bTParams b)
               , envClosure = envParams env `Map.union` envClosure env
               , envParams  = params
               } (unLL (body from))

-- | Generate the closure for bindings that have no type or closure arguments.
groupEnv :: Group (Bind a) -> Closure
groupEnv g = case g of
  NonRecursive b -> bind b
  Recursive bs   -> F.foldMap bind bs
  where
  -- only bind monomorphic value definitions
  bind b | null (bTParams b) && null (bCParams b) && null (bParams b)
           = Map.singleton (bName b) (bResult b)
         | otherwise
           = Map.empty


-- Traversal -------------------------------------------------------------------

-- | Lambda lift the expressions of a module.
--
-- XXX this doesn't currently perform lambda-lifting on the expressions that
-- form the body of a task.
llModule :: Module -> LL Module
llModule m =
  do binds' <- mapM (T.traverse (llTopBind llExpr)) (modBinds m)
     return m { modBinds = binds' }

-- | Top-level bindings don't get lifted.
llTopBind :: Types a => (a -> LL a) -> Bind a -> LL (Bind a)
llTopBind llBody bind = withParams bind $ \ subst ->
  do body' <- llBody (bBody bind)
     return bind { bBody = apply subst body' }


-- | Lambda lift declarations from within an expression.
llExpr :: Expr -> LL Expr
llExpr e = case e of

  ETApp (EVar n) ts -> rewriteCall n ts
  EVar n            -> rewriteCall n []


  EApp f x -> EApp <$> llExpr f <*> llExpr x

  ELet ds b ty -> llLet ds b ty

  EPrim {} -> return e

  ECon {} -> return e

  ECase src arms rty -> ECase src <$> llMatch arms <*> pure rty

  EStmts ity ty as -> EStmts ity ty <$> T.traverse llAction as

  ELit {} -> return e

  ETApp f ty -> ETApp <$> llExpr f <*> pure ty
  ECApp f cs -> ECApp <$> llExpr f <*> T.traverse llExpr cs

  EMkList ty es  -> EMkList ty <$> T.traverse llExpr es
  EMkTuple elems -> let (tys,es) = unzip elems
                     in EMkTuple . zip tys <$> T.traverse llExpr es
  ETopo {}       -> return e
  ETaskSet {}    -> return e
  ESchedule {}   -> return e

llLet :: [Group Decl] -> Expr -> Type -> LL Expr
llLet decls body ty = llDecls decls $ \ decls' ->
  do body' <- llExpr body
     return (letE decls' body' ty)

-- | Lambda lift a match, extending the closure with anything introduced.
llMatch :: Match Pattern -> LL (Match Pattern)
llMatch m = case m of

  MCase s sty m' -> MCase <$> llExpr s  <*> pure sty <*> llMatch m'

  MRename n e ty m' -> extendClosure (Map.singleton n ty) $
    MRename n <$> llExpr e <*> pure ty <*> llMatch m'

  MGuard g m' -> MGuard <$> llExpr g <*> llMatch m'

  MPat pat m' -> extendClosure (patClosure pat) (MPat pat <$> llMatch m')

  MSplit l r -> MSplit <$> llMatch l <*> llMatch r

  MFail -> pure m

  MExpr e -> MExpr <$> llExpr e

llAction :: Action -> LL Action
llAction a = case a of
  ABind mb e ty      -> ABind mb <$> llExpr e <*> pure ty
  AReceive r fs to w -> AReceive r <$> T.traverse llFrom fs
                                   <*> T.traverse llTimeout to
                                   <*> T.traverse llExpr w

llFrom :: From -> LL From
llFrom (From src msg msgTy body) = From src msg msgTy <$> llMatch body

llTimeout :: Timeout -> LL Timeout
llTimeout (Timeout to body) = Timeout to <$> llExpr body


-- Lambda Lifting --------------------------------------------------------------

-- | Calculate the closure that a binding requires.
bindClosure :: Decl -> LL Closure
bindClosure bind = withParams bind $ \ _ ->
  do clos <- getClosure (bBody bind)

     traceMsg $ hang (text "closure for" <+> pp (bName bind) <> char ':')
                   2 (ppClosure clos)

     return clos

-- | Generate the rewriting for a binding, given its closure.
bindRewrite :: Decl -> Closure -> Rewrite
bindRewrite b = rewrite (bName b)

-- | Lambda-lift a group of declarations, then run a continuation in their
-- modified environment, passing in the declarations that weren't lifted.
llDecls :: [Group Decl] -> ([Group Decl] -> LL a) -> LL a
llDecls gs k = go [] gs
  where
  go acc groups = case groups of

    g:rest -> do e <- llGroup g
                 case e of
                   Right g' -> extendClosure (groupEnv g') (go (g' : acc) rest)
                   Left rw  -> addRewrite rw (go acc rest)

    [] -> k (reverse acc)

-- | Lambda-lift a group of declarations.  This generates a rewrite when the
-- declarations have been lifted, or a new group when they are kept.
llGroup :: Group Decl -> LL (Either Rewrite (Group Decl))
llGroup g = case g of

  NonRecursive d ->
    do (mb,[d']) <- llBinds False [d]
       let g' = NonRecursive d'
       case mb of
         Nothing -> return (Right g')
         Just rw -> do emitGroup g'
                       return (Left rw)

  Recursive ds ->
    do (mb,ds') <- llBinds True ds
       let g'  = Recursive ds'
       case mb of
         Nothing -> return (Right g')
         Just rw -> do emitGroup g'
                       return (Left rw)

-- | Lambda-lift a group of declarations at one time.
--
-- XXX this currently treats all bindings as functions, so things like
--
--   let x = y + 1
--    in ...
--
-- will be lifted out to the top-level as functions of their closures.
llBinds :: Bool -> [Decl] -> LL (Maybe Rewrite,[Decl])
llBinds isRec ds =
  do clos <- mconcat `fmap` mapM bindClosure ds

     traceMsg $ hang (text "group closure:")
                   2 (ppClosure clos)

     -- expressions need lifting when they are part of a recursive block, or
     -- contain parameters
     let needsLifting   = isRec || any (not . null . bParams) ds

         -- expressions need rewriting when they're going to be lifted, and have
         -- a non-empty closure
         needsRewriting = needsLifting && not (Map.null clos)

     traceMsg (text "needs lifting:" <+> text (show needsLifting))

     -- generate the local rewrite
     let rw | needsRewriting = F.foldMap (`bindRewrite` clos) ds
            | otherwise      = Map.empty

         process b = withParams b $ \ from ->
           do body' <- addRewrite rw (llExpr (bBody b))

              -- substitute away all free type vars
              let tps      = closTParams clos
                  next     = [ length (bTParams b) .. ]
                  bind p i = (p, p { tpIndex = i })
                  binds    = zipWith bind tps next
                  bvs      = map snd binds
                  subst    = freeBinds [ (p, TVar (TVBound g)) | (p,g) <- binds]
                                `mappend` from

              if needsRewriting
                 then return b { bTParams = bTParams b ++ bvs
                               , bCParams = apply subst (closParams clos)
                               , bBody    = apply subst body' }

                 else return b { bBody = body' }

     ds' <- mapM process ds

     if needsLifting
        then return (Just rw, ds')
        else return (Nothing, ds')
