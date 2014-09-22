{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Mistral.ModuleSystem.Interface where

import Mistral.Driver
import Mistral.ModuleSystem.Name
import Mistral.TypeCheck.AST
import Mistral.Utils.PP
import Mistral.Utils.SCC

import           Control.Applicative ( Alternative(..) )
import           Control.Monad ( msum )
import qualified Data.ByteString.Lazy as L
import           Data.Foldable ( asum )
import           Data.List ( find )
import qualified Data.Map as Map
import           Data.Maybe ( maybeToList )
import           Data.Monoid ( Monoid(..) )
import           Data.Serialize ( Serialize, decodeLazy, encodeLazy )
import           GHC.Generics ( Generic )
import           MonadLib ( BaseM )


-- Querying --------------------------------------------------------------------

class HasIface iface where
  -- | Lookup a name defined by an interface.  This could be a fully-qualified
  -- name for a top-level type, or an unqualified name for something that's not
  -- exported.
  lookupBind :: Name -> iface -> Maybe IfaceBind

  -- | Lookup an exported type synonym.
  lookupTySyn :: Name -> iface -> Maybe TySyn

  -- | Lookup type information from an interface.
  lookupType :: Name -> iface -> Maybe IfaceType

  -- | Retrieve all assumptions from the interface.
  getInsts :: iface -> [Inst]


-- Interfaces ------------------------------------------------------------------

data Iface = Iface { ifaceModName :: Name
                   , ifaceDeps    :: [Name]
                     -- ^ Modules that this module depends on
                   , ifaceDatas   :: [Group Data]
                   , ifaceBinds   :: Map.Map Name IfaceBind
                   , ifaceTySyns  :: Map.Map Name TySyn
                   , ifaceTypes   :: Map.Map Name IfaceType
                   , ifaceInsts   :: [Inst]
                   } deriving (Show,Generic)

instance Serialize Iface

instance HasIface Iface where
  lookupBind  qn iface = Map.lookup qn (ifaceBinds  iface)
                     <|> lookupConstr qn iface
  lookupTySyn qn iface = Map.lookup qn (ifaceTySyns iface)
  lookupType  qn iface = Map.lookup qn (ifaceTypes  iface)
  getInsts       iface = ifaceInsts iface


-- | Bindings
data IfaceBind = IfacePrim Schema Prim
               | IfaceBind Schema
                 deriving (Show,Generic)

instance Serialize IfaceBind

-- | Type constructors
data IfaceType = IfaceType Name
                 deriving (Show,Generic)

instance Serialize IfaceType

parseIface :: L.ByteString -> Either String Iface
parseIface  = decodeLazy

renderIface :: Iface -> L.ByteString
renderIface  = encodeLazy

-- | Load an interface file.
loadIface :: BaseM drv Driver => Name -> drv (Either String Iface)
loadIface m =
  do let path = ifaceName m
     traceMsg (text "Loading interface:" <+> text path)
     mb <- getBytes path
     case mb of
       Just bytes -> return (parseIface bytes)
       Nothing    -> return (Left ("Unable to find interface file: " ++ path))

saveIface :: BaseM drv Driver => Iface -> drv Bool
saveIface iface =
  do let path = ifaceName (ifaceModName iface)
     traceMsg (text "Writing interface:" <+> text path)
     saveBytes (ifaceName (ifaceModName iface)) (renderIface iface)

-- | Manufacture an IfaceBind for a constructor.
lookupConstr :: Name -> Iface -> Maybe IfaceBind
lookupConstr qn iface =
  msum (concatMap (map findConstr . groupElems) (ifaceDatas iface))
  where
  findConstr d =
    do constr <- find (\c -> cName c == qn) (dConstrs d)
       let res = tapp (TCon (dName d)) [ TVar (TVBound p) | p <- dParams d ]
       return $ IfaceBind Forall { sParams = dParams d
                                 , sProps  = []
                                 , sType   = foldr tarrow res (cParams constr) }



-- Interface Collections -------------------------------------------------------

data IfaceTrie = IfaceTrie { itSub   :: Map.Map String IfaceTrie
                           , itIface :: Maybe Iface
                           } deriving (Show)

singletonIfaceTrie :: Iface -> IfaceTrie
singletonIfaceTrie iface = go (modNamespace (ifaceModName iface))
  where
  go ns = case ns of
    n:rest -> IfaceTrie (Map.singleton n (go rest)) Nothing
    []     -> IfaceTrie Map.empty (Just iface)

insertIface :: Iface -> IfaceTrie -> IfaceTrie
insertIface iface trie = singletonIfaceTrie iface `mappend` trie

lookupIface :: Namespace -> IfaceTrie -> Maybe Iface
lookupIface ns trie = case ns of

  n:rest ->
    do trie' <- Map.lookup n (itSub trie)
       lookupIface rest trie'

  [] -> itIface trie

getIfaces :: IfaceTrie -> [Iface]
getIfaces trie = maybeToList (itIface trie)
              ++ concatMap getIfaces (Map.elems (itSub trie))

-- `mappend` and `mconcat` are left-biased as to the interface that they keep in
-- the event of a conflict.  Really, it should probably just panic when that
-- happens.
instance Monoid IfaceTrie where
  mempty      = IfaceTrie { itSub = Map.empty, itIface = Nothing }

  mappend l r = IfaceTrie { itSub   = Map.unionWith mappend (itSub l) (itSub r)
                          , itIface = itIface l <|> itIface r }

  mconcat ts  = IfaceTrie { itSub   = Map.unionsWith mappend (map itSub ts)
                          , itIface = asum (map itIface ts) }

instance HasIface IfaceTrie where
  lookupBind  qn trie = lookupBind  qn =<< lookupIface (nameNamespace qn) trie
  lookupTySyn qn trie = lookupTySyn qn =<< lookupIface (nameNamespace qn) trie
  lookupType  qn trie = lookupType  qn =<< lookupIface (nameNamespace qn) trie
  getInsts       trie = concatMap getInsts (getIfaces trie)
