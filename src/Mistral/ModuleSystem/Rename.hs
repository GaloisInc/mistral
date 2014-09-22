{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The renamer has two purposes:
--
-- 1. Rename all bindings so that they can be safely floated out to the
--    top-level during lambda-lifting.
--
-- 2. Once the module system is in place, rename all symbols that are imported
--    to their global name.

module Mistral.ModuleSystem.Rename (
    renameModule
  ) where

import Mistral.Driver
import Mistral.ModuleSystem.Interface ( Iface(..) )
import Mistral.ModuleSystem.Name
           ( Namespace, Name(..), RealName(..), modNamespace, mkLocal
           , nReal, mapReal )
import Mistral.ModuleSystem.NameMap
import Mistral.Parser.AST
import Mistral.Utils.PP
import Mistral.Utils.Panic ( panic )
import Mistral.Utils.Source

import           Control.Applicative ( Applicative(..), (<$>), Alternative)
import           Control.Monad ( MonadPlus )
import qualified Data.Map         as Map
import           Data.Monoid ( Monoid(..) )
import qualified Data.Traversable as T
import           MonadLib
                     ( runM, ReaderT, ask, local, StateT, set, get, BaseM(..) )


-- | Rename a module, giving every identifier a unique global name.
--
-- TODO: once the module system is implemented, this should also accept a list
-- of interfaces to use during renaming.
renameModule :: [Iface] -> Module -> Driver Module
renameModule ifaces m = failErrs $
  do (m',_) <- runM (getRN (rename m)) ro rw
     traceMsg (pp m')
     return m'
  where
  ro = RO { roNames     = importNameMap ifaces (modImports m)
          , roSource    = Unknown
          , roNameSpace = modNamespace (locThing (modName m)) }
  rw = RW { rwFresh = mempty }


-- Renaming Monad --------------------------------------------------------------

data RO = RO { roNames :: NameMap
               -- ^ Renaming environment.
             , roSource :: Source
               -- ^ Current module namespace
             , roNameSpace :: Namespace
             }

data RW = RW { rwFresh :: Map.Map (Level,String) Int
               -- ^ Fresh name source.  The String corresponds to unqualified
               -- names, and the Int corresponds to the number of things defined
               -- with that name.
             } deriving (Show)

newtype RN a = RN { getRN :: ReaderT RO (StateT RW Driver) a
                  } deriving (Functor,Applicative,Monad,MonadPlus,Alternative)

instance BaseM RN Driver where
  inBase m = RN (inBase m)


rwNextIndex :: (Level,String) -> RW -> (Maybe Int,RW)
rwNextIndex s rw = case Map.lookup s (rwFresh rw) of
  Just i  -> (Just i,  rw { rwFresh = Map.adjust (+1) s (rwFresh rw) })
  Nothing -> (Nothing, rw { rwFresh = Map.insert   s  0 (rwFresh rw) })

rwFreshOrigin :: Namespace -> Origin -> RW -> (Origin,RW)
rwFreshOrigin ns o rw = case o of
  FromIdent l src n -> let (n',rw') = rwFreshIdent ns l n rw
                        in (FromIdent l src n', rw')

  -- parameters are only ever present at the value level
  FromParam src n -> let (n',rw') = rwFreshParam n rw
                      in (FromParam src n', rw')

  -- imported names don't need to be freshened
  FromImport{} -> (o, rw)

-- | Identifiers are always turned into global names.
rwFreshIdent :: Namespace -> Level -> Name -> RW -> (Name,RW)
rwFreshIdent ns l n rw = case nReal n of

  Local s -> case rwNextIndex (l,s) rw of
    (Just i,rw')  -> (mapReal (const (Global ns (s ++ show i))) n, rw')
    (Nothing,rw') -> (mapReal (const (Global ns s))             n, rw')

  -- globals don't get renamed.  this will need to change if we ever allow
  -- qualified imports, particularly things of the form:
  --
  --   import qualified Foo as Bar
  _       -> (n,rw)

-- | Parameters can optionally stay the same.
rwFreshParam :: Name -> RW -> (Name,RW)
rwFreshParam n rw = case nReal n of

  Local s -> case rwNextIndex (LValue,s) rw of
    (Just i,rw')  -> (mapReal (const (Local (s ++ show i))) n, rw')
    (Nothing,rw') -> (n, rw')

  _       -> (n,rw)


-- | Freshen the RHS of the mapping, and run a RN action where it shadows the
-- current environment.
withNames :: HasNameMap names => names -> RN a -> RN a
withNames names m =
  do names' <- freshenNames (nameMap names)
     RN $ do
       ro <- ask
       -- XXX add warnings for shadowing
       local ro { roNames = names' `shadowing` roNames ro } (getRN m)


-- | Add source location to a renaming context.
withSource :: Source -> RN a -> RN a
withSource src m = RN $
  do ro <- ask
     local ro { roSource = src } (getRN m)

-- | The closest source location.
srcLoc :: RN Source
srcLoc  = RN (roSource `fmap` ask)

-- | Add an error, using the current source location.
rnErr :: PPDoc -> RN ()
rnErr msg = RN $
  do ro <- ask
     addErrAt msg (roSource ro)

-- | Add a warning at the current source location.
--
-- XXX not used currently, but there are situations that we should be adding
-- warnings in (shadowing)
-- rnWarn :: PPDoc -> RN ()
-- rnWarn msg = RN $
--   do ro <- ask
--      addWarnAt msg (roSource ro)

-- | Freshen the renaming targets of a name map.
freshenNames :: NameMap -> RN NameMap
freshenNames nm = RN $
  do rw <- get
     ro <- ask
     let (rw', em') = Map.mapAccum (step (roNameSpace ro)) rw  (nmExpr nm)
         (rw'',tm') = Map.mapAccum (step (roNameSpace ro)) rw' (nmType nm)
     set rw''
     return NameMap { nmExpr = em', nmType = tm' }

  where
  step ns rw os = case os of
    [o] -> let (o',rw') = rwFreshOrigin ns o rw
            in (rw', [o'])

    -- a non-singleton list will cause an error, so don't do the work to freshen
    -- it.
    _    -> (rw,os)


-- | Freshen a single value-level name.
freshIdent :: Name -> RN Name
freshIdent nm = RN $
  do rw <- get
     ro <- ask
     let (n',rw') = rwFreshIdent (roNameSpace ro) LValue nm rw
     set rw'
     return n'

-- | Freshen a single global value-level name.
freshGlobal :: Name -> RN Name
freshGlobal nm = RN $
  do rw <- get
     ro <- ask
     let (n',rw') = rwFreshIdent (roNameSpace ro) LValue nm rw
     set rw'
     return n'


-- Renaming --------------------------------------------------------------------

rnPanic :: [String] -> a
rnPanic  = panic "Mistral.ModuleSystem.Rename"

-- | Given an optional value-level name, produce a fresh name when none is
-- provided.
rnMbName :: Bool -> String -> Maybe (Located Name) -> RN (Maybe (Located Name))
rnMbName isGlobal template mb = case mb of
  Just n  ->
    Just `fmap` T.mapM rnExpr n

  Nothing ->
    do n   <- if isGlobal
                 then freshGlobal (mkLocal template)
                 else freshIdent  (mkLocal template)
       src <- srcLoc
       return (Just (n `at` src))

-- | Given an expression-level name, lookup its renaming.
rnExpr :: Name -> RN Name
rnExpr n =
  do ro <- RN ask
     case Map.lookup n (nmExpr (roNames ro)) of
       Just [o] -> return (getName o)

       Just []   -> rnPanic [ "Invalid renaming environment" ]

       Just ns   ->
         do rnErr (nameClash n ns)
            return n

       Nothing   ->
         do rnErr (unknownIdent n)
            return n


-- | Rename a variable introduced by a pattern.
rnVarBind :: Name -> RN Name
rnVarBind n =
  do ro <- RN ask
     case Map.lookup n (nmExpr (roNames ro)) of
       Just [o]  -> return (getName o)
       Nothing   -> freshIdent n
       Just []   -> rnPanic [ "Invalid renaming environment" ]
       Just ns   -> do rnErr (nameClash n ns)
                       return n


nameClash :: Name -> [Origin] -> PPDoc
nameClash n ns =
  hang (text "Multiple names for" <+> backquotes (pp n))
     2 (vcat (map pp ns))

unknownIdent :: Name -> PPDoc
unknownIdent n = text "Not in scope:" <+> backquotes (pp n)

-- | Given an type-level name, lookup its renaming.
rnType :: Name -> RN Name
rnType n =
  do ro <- RN ask
     case Map.lookup n (nmType (roNames ro)) of
       Just [o] -> return (getName o)

       Just []   -> rnPanic [ "Invalid renaming environment" ]

       Just ns   ->
         do rnErr (nameClash n ns)
            return n

       Nothing   ->
         do rnErr (unknownIdent n)
            return n

-- | Rename a located thing.
rnLoc :: (a -> RN a) -> Located a -> RN (Located a)
rnLoc rn loc = withSource (getSource loc) $
  do a' <- rn (locThing loc)
     return loc { locThing = a' }


-- | Rename parsed names to internal names.
class Rename a where
  rename :: a -> RN a

instance Rename a => Rename (Maybe a) where
  rename = T.mapM rename

instance Rename a => Rename (Located a) where
  rename = T.mapM rename

instance (Rename a) => Rename [a] where
  rename = T.mapM rename

instance Rename Module where
  rename m =
    do ds' <- withNames m (mapM rename (modDecls m))
       return m { modDecls = ds' }

instance Rename TopDecl where
  rename td = case td of
    TDecl d -> TDecl <$> rename d
    TData d -> TData <$> rename d

instance Rename Data where
  rename d =
    do n'  <- rnLoc rnType (dName d)
       cs' <- rename (dConstrs d)
       return d { dName = n', dConstrs = cs' }

instance Rename Constr where
  rename c =
    do n'  <- rnLoc rnExpr (cName c)
       ps' <- rename (cParams c)
       return c { cName = n', cParams = ps' }

instance Rename a => Rename (Decl a) where
  rename d = case d of
    DBind b      -> DBind     <$> rename b
    DSig s       -> DSig      <$> rename s
    DSource s d' -> DSource s <$> rename d'

-- NOTE: it's assumed that the binding will be renamed by its context, top-level
-- or let-binding; this instance will not rename the actual binding name.
instance Rename a => Rename (Bind a) where
  rename b = withNames (paramMap (bParams b)) $
    do n'   <- rnLoc rnExpr (bName b)
       sig' <- rename (bSig b)
       ps'  <- mapM (T.mapM rnExpr) (bParams b)
       b'   <- rename (bBody b)
       return b { bName   = n'
                , bSig    = sig'
                , bParams = ps'
                , bBody   = b' }

instance Rename Signature where
  rename sig =
    do ns' <- mapM (rnLoc rnExpr) (sigNames sig)
       ty' <- rename (sigSchema sig)
       return Signature { sigNames  = ns'
                        , sigSchema = ty' }

instance Rename a => Rename (Comp a) where
  rename c = withNames (compArms c)
               (Comp <$> (rename (compResult c))
                     <*> mapM rnCompArm (compArms c)
                     <*> rename (compTags c)
                     <*> pure   (compSource c))

rnCompArm :: CompArm -> RN CompArm
rnCompArm arm = case arm of
  a:as -> withNames a $ do a'  <- rename a
                           as' <- rnCompArm as
                           return (a':as')
  []   -> return []

instance Rename CompStmt where
  rename cs = case cs of
    CSGen p e        -> CSGen        <$> rename p <*> rename e
    CSGuard e        -> CSGuard      <$> rename e
    CSSource src cs' -> withSource src (CSSource src <$> rename cs')

instance Rename Topology where
  rename t = withNames (topoElements t)
           $ Topology <$> rename (topoElements t)

instance Rename TopoStmt where
  rename ts = case ts of
    TSNode nm n      -> TSNode       <$> rnMbName False "node" nm <*> rename n
    TSLink l         -> TSLink       <$> rename l
    TSUsing e ts'    -> TSUsing      <$> rename e  <*> rename ts'
    TSComp c         -> TSComp       <$> rename c
    TSSource src ts' -> withSource src (TSSource src <$> rename ts')

instance Rename Node where
  rename n = Node <$> rename (nSpec n) <*> rename (nTags n)

instance Rename Link where
  rename l = Link (linkType l) <$> rename (linkLeft l)
                               <*> rename (linkRight l)
                               <*> rename (linkTags l)

instance Rename LinkTarget where
  rename lt = case lt of
    LTName loc -> LTName <$> T.mapM rnExpr loc
    LTTag tag  -> LTTag  <$> rename tag


instance Rename TaskSet where
  rename ts = withNames ts (TaskSet <$> rename (tsTasks ts))


instance Rename TaskStmt where
  rename ts = case ts of
    TStTask mb t      -> TStTask       <$> rnMbName True "task" mb <*> rename t
    TStComp c         -> TStComp       <$> rename c
    TStSource src ts' -> withSource src (TStSource src <$> rename ts')

instance Rename Task where
  rename t = Task <$> rename (tBody t)
                  <*> rename (tConstraints t)
                  <*> rename (tTags t)

instance Rename TaskConstraint where
  rename tc = case tc of
    After s nm       -> After s      <$> rnExpr nm
    On e             -> On           <$> rename e
    TCSource src tc' -> withSource src (TCSource src <$> rename tc')


instance Rename Schedule where
  rename s = Schedule <$> rename (sStmts s)

instance Rename SchedStmt where
  rename s = case s of
    Using ts to    -> Using       <$> rename ts   <*> rename to
    STag stmt ts   -> STag        <$> rename stmt <*> rename ts
    SExpr e        -> SExpr       <$> rename e
    SSource src s' -> withSource src (SSource src <$> rename s')


instance Rename Pattern where
  rename p = case p of
    PVar n         -> PVar <$> rnVarBind n
    PCon n ps      -> PCon <$> rnExpr n <*> rename ps
    PLit _         -> return p
    PTuple ps      -> PTuple <$> rename ps
    PWildcard      -> return p
    PSource src p' -> withSource src (PSource src <$> rename p')

instance Rename Expr where
  rename e = case e of
    EApp f xs       -> EApp <$> rename f  <*> rename xs
    ELet ds e'      -> withNames ds (ELet <$> rename ds <*> rename e')
    ELam ps e'      -> rnLambda ps e'
    EVar n          -> EVar <$> rnExpr n
    ECon n          -> ECon <$> rnExpr n
    ETuple es       -> ETuple <$> mapM rename es
    ELit _          -> return e
    EList es        -> EList    <$> mapM rename es
    EIf b t f       -> EIf      <$> rename b  <*> rename t <*> rename f
    EActions a      -> EActions <$> renameStmts a
    ETransitions ts -> ETransitions <$> rename ts
    ECase m rty     -> ECase <$> rename m <*> rename rty
    ESource s e'    -> withSource s (ESource s <$> rename e')
    ETopology t     -> ETopology <$> rename t
    ETaskSet ts     -> ETaskSet  <$> rename ts
    ESchedule s     -> ESchedule <$> rename s

    -- wildcard patterns should not be present at this point
    EWildPat -> do rnErr (text "Pattern syntax in expression context")
                   return EWildPat

instance Rename CaseArm where
  rename ca = withNames (cPat ca) $
    do pat'  <- rename (cPat ca)
       body' <- rename (cBody ca)
       return CaseArm { cPat = pat', cBody = body' }

-- | Rename a lambda to a let-bound function.
rnLambda :: [Located Name] -> Expr -> RN Expr
rnLambda ps e = withNames (paramMap ps) $
  do n <- freshIdent (mkLocal "lam")
     -- the bound name has no source location, as it doesn't originate in the
     -- source.
     e'  <- rename e
     ps' <- T.mapM (rnLoc rnExpr) ps
     let bind = Bind { bName   = n `at` Unknown
                     , bSig    = Nothing
                     , bParams = ps'
                     , bBody   = e' }
     return (ELet [DBind bind] (EVar n))

-- | Rename a list of statements, carrying forward the environment that each
-- statement defines.
renameStmts :: [Stmt] -> RN [Stmt]
renameStmts stmts = case stmts of
  s:rest -> withNames s $
    do s'    <- rename s
       rest' <- renameStmts rest
       return (s':rest')
  [] -> return []

instance Rename Stmt where
  rename s = case s of
    SBind p e       -> SBind   <$> rnLoc rnExpr p <*> rename e
    SAssign p e     -> SAssign <$> rnLoc rnExpr p <*> rename e
    SSeq e          -> SSeq    <$> rename e
    SLet ds         -> SLet    <$> rename ds
    StSource src s' -> withSource src (StSource src <$> rename s')

instance Rename Transition where
  rename tr = withNames (trPattern tr) $
    do tp' <- rename (trPattern tr)
       b'  <- rename (trBody tr)
       return Transition { trPattern = tp'
                         , trBody    = b' }

instance Rename TPattern where
  rename tp = case tp of
    Receive ts p   -> Receive <$> rename ts <*> rename p
    Timeout time   -> Timeout <$> rename time
    At time        -> At      <$> rename time
    TPSource s tp' -> withSource s (TPSource s <$> rename tp')

instance Rename SourcePred where
  rename sp = case sp of
    SrcBind n            -> SrcBind       <$> rnVarBind n
    SrcTaskHandle n      -> SrcTaskHandle <$> rnExpr n
    SrcTags t            -> SrcTags       <$> rename t
    SrcAny               -> return sp

instance Rename TagOp where
  rename to = case to of
    TOIntersect a b -> TOIntersect <$> rename a <*> rename b
    TOUnion a b     -> TOUnion <$> rename a <*> rename b
    TOTag t         -> TOTag <$> rename t

instance Rename Tag where
  rename tag = case tag of
    TagAtom _ -> pure tag
    TagVar  n -> TagVar <$> rnExpr n

instance Rename Schema where
  rename (Forall ps qs ty s) = Forall ps <$> rename qs <*> rename ty <*> pure s

instance Rename Type where
  rename ty = case ty of
    TApp f xs       -> TApp        <$> rename f <*> rename xs
    TFun a b        -> TFun        <$> rename a <*> rename b
    TVar _          -> pure ty
    TCon n          -> TCon        <$> rnType n
    TTuple ts       -> TTuple      <$> rename ts
    TList t         -> TList       <$> rename t
    TSource src ty' -> withSource src (TSource src <$> rename ty')
