module Mistral.ModuleSystem.NameMap where

import           Mistral.ModuleSystem.Interface ( Iface(..) )
import           Mistral.ModuleSystem.Name ( Name, unqual )
import           Mistral.Parser.AST
import qualified Mistral.TypeCheck.AST as Core
import           Mistral.Utils.PP
import           Mistral.Utils.Source
import           Mistral.Utils.SCC

import           Data.Foldable ( foldMap )
import           Data.List ( nub, find )
import qualified Data.Map as Map
import           Data.Monoid ( Monoid(..) )


-- Name Management -------------------------------------------------------------

-- | The level a name is defined at.
data Level = LType
           | LValue
             deriving (Show,Eq,Ord)

instance PP Level where
  ppr LType  = text "type"
  ppr LValue = empty


data Origin = FromIdent Level Source Name
              -- ^ Identifier defined in the current module
            | FromParam Source Name
              -- ^ Function parameter.  NOTE: these are assumed to always come
              -- from the value level (LValue)
            | FromImport Level Name Name
              deriving (Show,Eq)

instance PP Origin where
  ppr o = case o of
    FromIdent l src n -> ppr l            <+> ppr n <+> text "from" <+> ppr src
    FromParam   src n -> text "parameter" <+> ppr n <+> text "from" <+> ppr src
    FromImport l  m n -> ppr l            <+> ppr n <+> text "from module" <+> ppr m

-- | The namespace that a name comes from.
originLevel :: Origin -> Level
originLevel (FromIdent l _ _)  = l
originLevel (FromParam _ _)    = LValue
originLevel (FromImport l _ _) = l

-- | Retrieve the name associated with an origin.
getName :: Origin -> Name
getName o = case o of
  FromIdent  _ _ n -> n
  FromParam    _ n -> n
  FromImport _ _ n -> n

fromType :: Located Name -> Origin
fromType ln = FromIdent LType (getSource ln) (locThing ln)

fromIdent :: Located Name -> Origin
fromIdent ln = FromIdent LValue (getSource ln) (locThing ln)

fromParam :: Located Name -> Origin
fromParam ln = FromParam (getSource ln) (locThing ln)


-- Name Maps -------------------------------------------------------------------

-- | NameMaps contain a mapping between names as they appear in the source, and
-- names as they should appear after renaming.
data NameMap = NameMap { nmExpr :: Map.Map Name [Origin]
                       , nmType :: Map.Map Name [Origin]
                       } deriving (Show)

instance Monoid NameMap where
  {-# INLINE mempty #-}
  mempty      = NameMap { nmExpr = Map.empty
                        , nmType = Map.empty }

  {-# INLINE mappend #-}
  mappend l r = mconcat [l,r]

  -- merge by producing a conflict
  mconcat ns  = NameMap { nmExpr = merge nmExpr
                        , nmType = merge nmType }
    where
    merge p = Map.unionsWith conflict (map p ns)


-- | Merge two lists of names, considering duplicates to not conflict.  For
-- example:
--
-- import Foo
-- import Foo
--
-- shouldn't produce conflicts.
conflict :: [Origin] -> [Origin] -> [Origin]
conflict as bs = nub (as ++ bs)

singleton :: Name -> Origin -> NameMap
singleton from to = case originLevel to of
  LType  -> mempty { nmType = Map.singleton from [to] }
  LValue -> mempty { nmExpr = Map.singleton from [to] }

-- | Merge two naming environments, preferring names on the left.
shadowing :: NameMap -> NameMap -> NameMap
shadowing l r = NameMap { nmExpr = merge nmExpr, nmType = merge nmType }
  where
  -- Map.union is left-biased
  merge p = Map.union (p l) (p r)


-- NameMap Construction --------------------------------------------------------

-- | The name map for a list of arguments.
paramMap :: [Located Name] -> NameMap
paramMap  = foldMap (\ ln -> singleton (locThing ln) (fromParam ln) )

-- | Generate the name map for a set of import declarations, given the
-- interfaces that they'll require.
importNameMap :: [Iface] -> [Import] -> NameMap
importNameMap ifaces = foldMap $ \ imp ->
  case find ((impModule imp ==) . ifaceModName) ifaces of
    Just iface -> importEnv imp iface
    Nothing    -> mempty

-- XXX currently, imports are simple enough that they don't modify the
-- environment produced by an interface.
importEnv :: Import -> Iface -> NameMap
importEnv _imp iface = nameMap iface



nameMap :: HasNameMap a => a -> NameMap
nameMap  = nameMapSource Unknown

class HasNameMap a where
  nameMapSource :: Source -> a -> NameMap

instance HasNameMap NameMap where
  {-# INLINE nameMapSource #-}
  nameMapSource _ m = m

instance HasNameMap a => HasNameMap (Maybe a) where
  nameMapSource src = foldMap (nameMapSource src)

instance HasNameMap a => HasNameMap [a] where
  nameMapSource src = foldMap (nameMapSource src)

instance HasNameMap a => HasNameMap (Located a) where
  nameMapSource _ loc = nameMapSource (getSource loc) (locThing loc)

instance HasNameMap a => HasNameMap (Group a) where
  nameMapSource src = foldMap (nameMapSource src)


-- by itself, the interface maps the unqualified, and fully qualified names that
-- it defines to its fully qualified names, so
--
-- pure         ~> Prelude.pure
-- Prelude.pure ~> Prelude.pure
instance HasNameMap Iface where
  nameMapSource _ iface =
    mconcat [ foldMap bindEx (Map.keys (ifaceBinds  iface))
            , foldMap bindTy (Map.keys (ifaceTySyns iface))
            , foldMap bindTy (Map.keys (ifaceTypes  iface))
            , foldMap dataGroup        (ifaceDatas  iface) ]
    where
    bindEx n = singleton n          (FromImport LValue (ifaceModName iface) n) `mappend`
               singleton (unqual n) (FromImport LValue (ifaceModName iface) n)

    bindTy n = singleton n          (FromImport LType (ifaceModName iface) n) `mappend`
               singleton (unqual n) (FromImport LType (ifaceModName iface) n)

    dataGroup = foldMap dataMap
    dataMap d = mconcat $ bindTy (Core.dName d)
                        : map (bindEx . Core.cName) (Core.dConstrs d)


-- The name map generated by a module is the naming map from the unqualified
-- versions of all the names the module defines to ones that are fully qualified
-- by the module namespace.
--
-- INVARIANT: parsed declarations can only ever define locally-bound names, not
-- fully-qualified names.
instance HasNameMap Module where
  nameMapSource _ m = mconcat
    [ mconcat [ new fromIdent ln | d  <- topDecls tds
                                 , ln <- binds d ]
    , foldMap dataNames (topDatas tds)
    ]
    where
    tds = splitTopDecls (modDecls m)

    binds d = case d of
      DBind b      -> [bName b]
      DSig _       -> []
      DSource _ d' -> binds d'

    new mk ln = singleton (locThing ln) (mk ln)

    dataNames d =
      mconcat (new fromType (dName d) : map (new fromIdent . cName) (dConstrs d))

instance HasNameMap Pattern where
  nameMapSource src p = case p of
    PVar n          -> singleton n (fromParam (n `at` src))
    PCon _ ps       -> nameMapSource src ps
    PLit _          -> mempty
    PTuple ps       -> nameMapSource src ps
    PWildcard       -> mempty
    PSource src' p' -> nameMapSource src' p'

instance HasNameMap TPattern where
  nameMapSource src tp = case tp of
    Receive sp rp     -> nameMapSource src sp `mappend` nameMapSource src rp
    Timeout _         -> mempty
    At _              -> mempty
    TPSource src' tp' -> nameMapSource src' tp'

instance HasNameMap SourcePred where
  nameMapSource src sp = case sp of
    SrcBind n            -> singleton n (fromParam (n `at` src))
    SrcTaskHandle _      -> mempty
    SrcTags _            -> mempty
    SrcAny               -> mempty

instance HasNameMap Tag where
  nameMapSource src tag = case tag of
    TagAtom _ -> mempty
    TagVar n  -> singleton n (fromParam (n `at` src))

instance HasNameMap Stmt where
  nameMapSource src s = case s of
    SBind ln _       -> singleton (locThing ln) (fromParam ln)
    SAssign ln _     -> singleton (locThing ln) (fromParam ln)
    SSeq _           -> mempty
    SLet ds          -> nameMapSource src ds
    StSource src' s' -> nameMapSource src' s'

instance HasNameMap (Decl a) where
  nameMapSource src d = case d of
    DBind b         -> nameMapSource src b
    DSig _          -> mempty -- signatures don't bind names
    DSource src' d' -> nameMapSource src' d'

instance HasNameMap (Bind a) where
  nameMapSource _ b = singleton (locThing (bName b)) (fromIdent (bName b))

instance HasNameMap CompStmt where
  nameMapSource src stmt = case stmt of
    CSGen p _           -> nameMapSource src p
    CSGuard _           -> mempty
    CSSource src' stmt' -> nameMapSource src' stmt'

instance HasNameMap TopoStmt where
  nameMapSource _src stmt = case stmt of
    TSNode mb _         -> mbLNameMap mb
    TSLink _            -> mempty
    TSUsing _ _         -> mempty
    TSComp _            -> mempty
    TSSource src' stmt' -> nameMapSource src' stmt'

instance HasNameMap TaskSet where
  nameMapSource src ts = nameMapSource src (tsTasks ts)

-- | Construct a name map for optional identifiers.
--
-- NOTE: this produces name maps that cover values, not types.
mbLNameMap :: Maybe (Located Name) -> NameMap
mbLNameMap  = foldMap (\ ln -> singleton (locThing ln) (fromIdent ln) )

instance HasNameMap TaskStmt where
  nameMapSource _src stmt = case stmt of
    TStTask mb _         -> mbLNameMap mb
    TStComp _            -> mempty
    TStSource src' stmt' -> nameMapSource src' stmt'
