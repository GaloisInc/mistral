{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Mistral.TypeCheck.Unify (
    -- * Unification
    mgu
  , match
  , Skolems
  , UnifyError(..)

    -- * Substitution
  , Subst()
  , freeBind,  freeBinds
  , boundBind, boundBinds
  , Types(..)
  , apply
  ) where

import Mistral.TypeCheck.AST
import Mistral.Utils.PP
import Mistral.Utils.SCC

import           Data.Foldable ( foldMap )
import           Data.Monoid ( Monoid(..) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import           MonadLib
                     ( runId, ExceptionM, RunExceptionM, runExceptionT, raise )
import qualified MonadLib as M


-- Unification -----------------------------------------------------------------

data UnifyError = UnifyError Type Type
                  -- ^ Type 1 and Type 2 do not unify
                | OccursCheckFailed Type Type
                  -- ^ Type 1 occurs in Type 2
                | InvalidSkolemBinding TParam
                  -- ^ Tried to bind a skolem variable
                  deriving (Show)

instance PP UnifyError where
  ppr ue = case ue of

    UnifyError l r ->
      fsep [ text "couldn't match the expected type", backquotes (pp l)
           , text "with actual type", backquotes (pp r) ]

    OccursCheckFailed l r ->
      hang (text "cannot construct the infinite type:")
         2 (pp l <+> char '=' <+> pp r)

    InvalidSkolemBinding p ->
      hsep [ text "type variable", backquotes (pp p)
           , text "may only be bound to itself" ]


-- | Skolem variables, not to be bound in a substitution.
type Skolems = Set.Set TParam

-- | Compute the matching substitution between two types, such that
--
--  apply (match t1 t2) t1 = t2
match :: Skolems -> Type -> Type -> Either UnifyError Subst
match skolems t1 t2 = runId (runExceptionT (go t1 t2))
  where
  go l r = case (l,r) of

    (TCon a, TCon b) | a == b -> return mempty

    -- two of the same synonym instance
    (TSyn n ts _, TSyn n' ts' _) | n == n' && ts == ts' -> return mempty

    -- synonyms and anything else
    (TSyn _ _ t, _) -> saveError l r (go t r)
    (_, TSyn _ _ t) -> saveError l r (go l t)

    -- type applications unify pointwise
    (TApp a b, TApp x y) ->
      do u1 <- go a x
         u2 <- go (apply u1 b) (apply u1 y)
         return (u2 `mappend` u1)

    -- only free variables will produce a binding substitution.  this is the
    -- difference with mgu, only bind variables that occur from the type on the
    -- left.
    (TVar (TVFree v), _) -> varBind skolems v l r

    -- bound variables only unify with themselves.
    (TVar (TVBound a), TVar (TVBound b)) | a == b -> return mempty

    -- the types do not unify
    _ -> raise (UnifyError l r)


-- | Compute the most-general unifier for two types.  The first type is the
-- constructed type (variables, etc) and the second will usually come from a
-- user signature.
mgu :: Skolems -> Type -> Type -> Either UnifyError Subst
mgu skolems t1 t2 = runId (runExceptionT (go t1 t2))
  where
  go l r = case (l,r) of

    -- two constructors unify when their constructor names are the same
    --
    -- TODO: check that the kinds are the same, if we decide to add kind
    --       checking
    (TCon a, TCon b) | a == b -> return mempty

    -- two of the same synonym instance
    (TSyn n ts _, TSyn n' ts' _) | n == n' && ts == ts' -> return mempty

    -- synonyms and anything else
    (TSyn _ _ t, _) -> saveError l r (go t r)
    (_, TSyn _ _ t) -> saveError l r (go l t)

    -- type applications unify pointwise
    (TApp a b, TApp x y) ->
      do u1 <- go a x
         u2 <- go (apply u1 b) (apply u1 y)
         return (u2 `mappend` u1)

    -- skolems on the left may bind to vars on the right
    (TVar (TVFree a), TVar (TVFree b))
      | a `Set.member` skolems -> varBind skolems b r l

    -- only free variables will produce a binding substitution
    (TVar (TVFree v), _) -> varBind skolems v l r
    (_, TVar (TVFree v)) -> varBind skolems v r l

    -- bound variables only unify with themselves.
    (TVar (TVBound a), TVar (TVBound b)) | a == b -> return mempty

    -- the types do not unify
    _ -> raise (UnifyError l r)

-- | Produce a substitution from a variable and a type
--
-- TODO: check that the kinds are the same, if we decide to add kind
--       checking
varBind :: ExceptionM m UnifyError
        => Skolems -> TParam -> Type -> Type -> m Subst
varBind skolems v vt t
    -- the variable is the same as the type it is unified with
  | vt == t         = return mempty

    -- v occurs in t
  | occursCheck v t = raise (OccursCheckFailed vt t)

    -- v is a skolem variable
  | v `Set.member` skolems = raise (InvalidSkolemBinding v)

  | otherwise       = return (v `freeBind` t)

saveError :: RunExceptionM m UnifyError => Type -> Type -> m a -> m a
saveError l r m = do res <- M.try m
                     case res of
                       Right a -> return a
                       Left _  -> raise (UnifyError l r)


-- | True when the type variable occurs in the type given, indicating an
-- infinite type.
occursCheck :: Types ty => TParam -> ty -> Bool
occursCheck v ty = v `Set.member` typeVars ty


-- Type Substitution -----------------------------------------------------------

-- | Substitutions over bound and free variables.
data Subst = Subst { substFree, substBound :: Map.Map Int Type
                   } deriving (Show)

instance Monoid Subst where
  mempty        = Subst { substBound = Map.empty
                        , substFree  = Map.empty }
  mappend l r   = Subst { substBound = merge substBound
                        , substFree  = merge substFree }
    where
    -- apply the left substitution to everything on the right, and merge with
    -- the left.
    merge p = fmap (apply l) (p r) `Map.union` p l

instance PP Subst where
  ppr u = text "Bound Vars:"
       $$ ppBinds (substBound u)
       $$ text "Free Vars:"
       $$ ppBinds (substFree u)
    where
    ppBinds bs = vcat [ pp p <+> char '~' <+> pp ty | (p,ty) <- Map.toList bs ]

isEmptySubst :: Subst -> Bool
isEmptySubst u = Map.null (substFree u) && Map.null (substBound u)

-- | Generate a sigleton substitution with one free-variable binding.
freeBind :: TParam -> Type -> Subst
freeBind p ty = mempty { substFree = Map.singleton (tpIndex p) ty }

-- | Build a free variable substitution from a list.
freeBinds :: [(TParam,Type)] -> Subst
freeBinds us =
  mempty { substFree = Map.fromList [ (tpIndex p, ty) | (p,ty) <- us ] }

-- | Generate a sigleton substitution with one bound-variable binding.
boundBind :: TParam -> Type -> Subst
boundBind p ty = mempty { substBound = Map.singleton (tpIndex p) ty }

-- | Build a bound variable substitution from a list.
boundBinds :: [(TParam,Type)] -> Subst
boundBinds us =
  mempty { substBound = Map.fromList [ (tpIndex p, ty) | (p,ty) <- us ] }


-- | Apply the substitution.
apply :: Types a => Subst -> a -> a
apply u | isEmptySubst u = id
        | otherwise      = applyBndrs 0 u

-- | Substitute using the bound substitution.
--
-- XXX this likely contains a capture bug when the RHS of a substitution
-- contains a bound variable.
applyBound :: Int -> Subst -> TParam -> Maybe Type
applyBound bndrs u tp =
  adjustBound bndrs `fmap` Map.lookup (tpIndex tp - bndrs) (substBound u)

-- | Substitute using the free substitution.
--
-- XXX this likely contains a capture bug when the RHS of a substitution
-- contains a bound variable.
applyFree :: Int -> Subst -> TParam -> Maybe Type
applyFree bndrs u tp =
  adjustBound bndrs `fmap` Map.lookup (tpIndex tp) (substFree u)

-- | Increment the indices of bound variables substituted in, to avoid capture.
adjustBound :: Int -> Type -> Type
adjustBound 0 = id
adjustBound b = go
  where
  go ty = case ty of
    TApp l r         -> TApp (go l) (go r)
    TSyn s ts t'     -> TSyn s (map go ts) (go t')
    TVar (TVBound p) -> TVar (TVBound p { tpIndex = tpIndex p + b })
    _                -> ty

class Types a where
  -- | The free variables present in a type.
  typeVars   :: a -> Set.Set TParam

  -- | Apply the substitution, assuming that n bindings have been crossed.
  applyBndrs :: Int -> Subst -> a -> a

instance Types () where
  typeVars _       = Set.empty
  applyBndrs _ _ _ = ()

instance (Types a, Types b) => Types (a,b) where
  typeVars  (a,b)      = Set.unions [typeVars a,  typeVars b]
  applyBndrs i u (a,b) = (applyBndrs i u a, applyBndrs i u b)

instance (Types a, Types b, Types c) => Types (a,b,c) where
  typeVars  (a,b,c)      = Set.unions [typeVars a,  typeVars b,  typeVars c]
  applyBndrs i u (a,b,c) = (applyBndrs i u a, applyBndrs i u b, applyBndrs i u c)

instance (Types a, Types b, Types c, Types d) => Types (a,b,c,d) where
  typeVars  (a,b,c,d)      = Set.unions [ typeVars a,  typeVars b,  typeVars c
                                        , typeVars d ]
  applyBndrs i u (a,b,c,d) = ( applyBndrs i u a, applyBndrs i u b
                             , applyBndrs i u c, applyBndrs i u d)

instance (Types a, Types b, Types c, Types d, Types e)
      => Types (a,b,c,d,e) where
  typeVars  (a,b,c,d,e)    = Set.unions [ typeVars a,  typeVars b,  typeVars c
                                        , typeVars d, typeVars e ]
  applyBndrs i u (a,b,c,d,e) = ( applyBndrs i u a, applyBndrs i u b
                               , applyBndrs i u c, applyBndrs i u d
                               , applyBndrs i u e )

instance Types a => Types [a] where
  typeVars       = foldMap typeVars
  applyBndrs i u = fmap (applyBndrs i u)

instance Types a => Types (Maybe a) where
  typeVars       = foldMap typeVars
  applyBndrs i u = fmap (applyBndrs i u)

instance Types Type where
  typeVars ty = case ty of
    TApp l r         -> Set.union (typeVars l) (typeVars r)
    TSyn _ _ ty'     -> typeVars ty'
    TVar (TVFree tp) -> Set.singleton tp
    _                -> Set.empty

  applyBndrs b u ty = case ty of
    TApp l r -> TApp (applyBndrs b u l) (applyBndrs b u r)

    -- apply to the parameters, and the expanded form
    TSyn n ts ty' -> TSyn n (applyBndrs b u ts) (applyBndrs b u ty')

    -- substitute free and bound variables
    TVar (TVFree tp)  | Just ty' <- applyFree  b u tp -> ty'
    TVar (TVBound tp) | Just ty' <- applyBound b u tp -> ty'

    -- no changes to make
    _ -> ty

instance Types Schema where
  typeVars  (Forall _ cs ty)       = typeVars  (cs,ty)
  applyBndrs b u (Forall ps cs ty) = Forall ps (go cs) (go ty)
    where
    go :: Types a => a -> a
    go  = applyBndrs (b + length ps) u


instance Types Expr where
  typeVars e = case e of
    EApp l r           -> typeVars (l,r)
    ELet ds e' ty      -> typeVars (ds,e',ty)
    EPrim _            -> Set.empty
    EVar _             -> Set.empty
    ECon _             -> Set.empty
    ECase _ cases rty  -> typeVars (cases,rty)
    EStmts i ty as     -> typeVars (i,ty,as)
    ELit _             -> Set.empty
    ETApp e' ty        -> typeVars (e',ty)
    ECApp e' cs        -> typeVars (e',cs)
    EMkTuple elems     -> typeVars elems
    EMkList ty elems   -> typeVars (ty,elems)
    ETopo t            -> typeVars t
    ETaskSet t         -> typeVars t
    ESchedule s        -> typeVars s

  applyBndrs b u e = case e of
    EApp l r           -> EApp (applyBndrs b u l)  (applyBndrs b u r)
    ELet ds e' ty      -> ELet (applyBndrs b u ds) (applyBndrs b u e')
                               (applyBndrs b u ty)
    EPrim _            -> e
    EVar _             -> e
    ECon _             -> e
    ECase s cases rty  -> ECase s (applyBndrs b u cases) (applyBndrs b u rty)
    EStmts i ty as     -> EStmts (applyBndrs b u i) (applyBndrs b u ty)
                                 (applyBndrs b u as)
    ELit _             -> e

    ETApp e' ty        -> ETApp (applyBndrs b u e') (applyBndrs b u ty)
    ECApp e' cs        -> ECApp (applyBndrs b u e') (applyBndrs b u cs)

    EMkTuple elems     -> EMkTuple (applyBndrs b u elems)
    EMkList ty elems   -> EMkList (applyBndrs b u ty) (applyBndrs b u elems)
    ETopo t            -> ETopo (applyBndrs b u t)
    ETaskSet t         -> ETaskSet (applyBndrs b u t)
    ESchedule s        -> ESchedule (applyBndrs b u s)

instance Types SourcePredicate where
    typeVars s = case s of
        SNoPred  _  -> Set.empty
        SSrcIs   _  -> Set.empty
        STagPred _  -> Set.empty
    applyBndrs _ _ s = case s of
        SNoPred  _  -> s
        SSrcIs   _  -> s
        STagPred _  -> s

instance Types pat => Types (Match pat) where
  typeVars m = case m of
    MCase s sty m'    -> typeVars (s,sty,m')
    MRename _ e ty m' -> typeVars (e,ty,m')
    MGuard g m'       -> typeVars (g,m')
    MPat pat m'       -> typeVars (pat,m')
    MSplit l r        -> typeVars (l,r)
    MFail             -> Set.empty
    MExpr e           -> typeVars e

  applyBndrs b u m = case m of
    MCase s sty m'    -> MCase (app s) (app sty) (app m')
    MRename n s ty m' -> MRename n (app s) (app ty) (app m')
    MGuard g m'       -> MGuard (app g) (app m')
    MPat pat m'       -> MPat (app pat) (app m')
    MSplit l r        -> MSplit (app l) (app r)
    MFail             -> m
    MExpr e           -> MExpr (app e)
    where
    app :: Types a => a -> a
    app  = applyBndrs b u

instance Types a => Types (Group a) where
  typeVars       = foldMap typeVars
  applyBndrs b u = fmap (applyBndrs b u)

instance Types a => Types (Bind a) where
  typeVars b       = typeVars  ( bType b, bCParams b, bParams b
                               , bBody b, bResult b )
  applyBndrs b u d = d { bType    = applyBndrs b u (bType d)
                       , bCParams = go bCParams
                       , bParams  = go bParams
                       , bBody    = go bBody
                       , bResult  = go bResult }
    where
    go p = applyBndrs (b + length (bTParams d)) u (p d)

instance Types Param where
  typeVars p       = typeVars (pType p)
  applyBndrs b u p = p { pType = applyBndrs b u (pType p) }

instance Types From where
  typeVars (From _ _ msgTy m)           = typeVars (msgTy,m)
  applyBndrs b u (From src msg msgTy m) =
    From src msg (applyBndrs b u msgTy) (applyBndrs b u m)

instance Types Timeout where
  typeVars (Timeout e body) = typeVars (e, body)
  applyBndrs b u (Timeout e body) = Timeout (applyBndrs b u e) (applyBndrs b u body)

instance Types Pattern where
  typeVars pat = case pat of
    PCon _ ps -> typeVars ps
    PTuple ps -> typeVars ps
    _         -> Set.empty

  applyBndrs b u pat = case pat of
    PCon n ps -> PCon n (applyBndrs b u ps)
    PTuple ps -> PTuple (applyBndrs b u ps)
    _         -> pat


instance Types Action where
  typeVars a = case a of
    ABind _ e ty          -> typeVars (e,ty)
    AReceive _ fs to wild -> typeVars (fs,to,wild)

  applyBndrs b u a = case a of
    ABind mb e ty         -> ABind mb (applyBndrs b u e)  (applyBndrs b u ty)
    AReceive r fs to wild -> AReceive r (applyBndrs b u fs) (applyBndrs b u to)
                                    (applyBndrs b u wild)

instance Types a => Types (Comp a) where
  typeVars c = Set.unions [ typeVars (compResult c)
                          , typeVars (compArms   c) ]

  applyBndrs b u c = c { compResult = applyBndrs b u (compResult c)
                       , compArms   = applyBndrs b u (compArms   c) }

instance Types a => Types (Named a) where
  typeVars on = typeVars (nValue on)
  applyBndrs b u on = applyBndrs b u `fmap` on

instance Types CompStmt where
  typeVars cs = case cs of
    CompGen p e -> typeVars (p,e)
    CompGuard e -> typeVars e

  applyBndrs b u cs = case cs of
    CompGen p e -> CompGen (applyBndrs b u p) (applyBndrs b u e)
    CompGuard e -> CompGuard (applyBndrs b u e)



instance Types Topo where
  typeVars t = Set.unions [ typeVars (topoNodes    t)
                          , typeVars (topoNodeGens t)
                          , typeVars (topoLinks    t)
                          , typeVars (topoLinkGens t) ]

  applyBndrs b u t = t { topoNodes    = applyBndrs b u (topoNodes    t)
                       , topoNodeGens = applyBndrs b u (topoNodeGens t)
                       , topoLinks    = applyBndrs b u (topoLinks    t)
                       , topoLinkGens = applyBndrs b u (topoLinkGens t) }

instance Types Node where
  typeVars n       = typeVars (nSpec n, nType n, nTags n, nTasks n)
  applyBndrs b u n = Node { nSpec  = applyBndrs b u (nSpec n)
                          , nType  = applyBndrs b u (nType n)
                          , nTags  = applyBndrs b u (nTags n)
                          , nTasks = applyBndrs b u (nTasks n) }

instance Types Link where
  typeVars l = typeVars (lTags l)
  applyBndrs b u l = l { lTags  = applyBndrs b u (lTags l) }



instance Types Tasks where
  typeVars ts = typeVars (taskTasks ts, taskTaskGens ts)
  applyBndrs b u ts = Tasks { taskTasks    = applyBndrs b u (taskTasks ts)
                            , taskTaskGens = applyBndrs b u (taskTaskGens ts) }

instance Types Task where
  typeVars t = typeVars (tBody t, tTags t)
  applyBndrs b u t = Task { tBody        = applyBndrs b u (tBody t)
                          , tConstraints = applyBndrs b u (tConstraints t)
                          , tTags        = applyBndrs b u (tTags t) }

instance Types TaskConstraint where
  typeVars tc = case tc of
    TCOn ty e -> typeVars (ty,e)

  applyBndrs b u tc = case tc of
    TCOn ty e -> TCOn (applyBndrs b u ty) (applyBndrs b u e)


instance Types Sched where
  typeVars s = typeVars (schedStmts s)
  applyBndrs b u s = Sched { schedStmts = applyBndrs b u (schedStmts s) }

instance Types SchedStmt where
  typeVars (SchedUsing s o ts)       = typeVars (s,o,ts)
  applyBndrs b u (SchedUsing s o ts) = SchedUsing (applyBndrs b u s)
                                                  (applyBndrs b u o)
                                                  (applyBndrs b u ts)

instance Types Data where
  typeVars d       = typeVars (dConstrs d)
  applyBndrs b u d =
    d { dConstrs = applyBndrs (b + length (dParams d)) u (dConstrs d) }

instance Types Constr where
  typeVars c       = typeVars (cParams c)
  applyBndrs b u c = c { cParams = applyBndrs b u (cParams c) }
