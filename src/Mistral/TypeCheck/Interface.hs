module Mistral.TypeCheck.Interface (
    genIface
  ) where

import Mistral.ModuleSystem.Interface
import Mistral.TypeCheck.AST
import Mistral.Utils.SCC ( groupElems )

import           Data.Foldable ( foldMap )
import qualified Data.Map as Map


-- | Generate an interface from a core module.
genIface :: Module -> Iface
genIface m = Iface { ifaceModName = modName m
                   , ifaceDeps    = modDeps m
                   , ifaceDatas   = modDatas m
                   , ifaceBinds   = modTypes m
                   , ifaceTySyns  = Map.empty
                   , ifaceTypes   = Map.empty
                   , ifaceInsts   = modInsts m }

-- | Generate the binding map for a core module.
modTypes :: Module -> Map.Map Name IfaceBind
modTypes m = foldMap Map.fromList [ concatMap (map modBindType . groupElems) (modBinds m) ]

modBindType :: Bind a -> (Name, IfaceBind)
modBindType b = (bName b, IfaceBind (bType b))
