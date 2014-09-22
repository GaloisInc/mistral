module Mistral.ModuleSystem (
    moduleSystem
  , getImportIface
  , saveIface
  ) where

import Mistral.Driver
import Mistral.ModuleSystem.Interface
           ( Iface(..), loadIface, saveIface, IfaceTrie, insertIface )
import Mistral.ModuleSystem.Prelude ( prelude )
import Mistral.ModuleSystem.Rename ( renameModule )
import Mistral.Parser.AST ( Module(..), Import(..) )
import Mistral.Utils.PP

import Control.Monad ( mzero )
import Data.Monoid ( mempty )


-- | Rename a module using its import environment.
moduleSystem :: Module -> Driver (Module,IfaceTrie)
moduleSystem m = phase "rn" $
  do ifaces <- failErrs (tryMapM getImportIface (modImports m))
     m'     <- renameModule ifaces m
     traceMsg (pp m')
     return (m',foldr insertIface mempty ifaces)

-- | Load the interface specified by an import declaration.
getImportIface :: Import -> Driver Iface
getImportIface i
  | name == ifaceModName prelude = return prelude
  | otherwise                    =
     do traceMsg (text "Loading interface for:" <+> pp name)
        e <- loadIface name
        case e of
          Right iface -> return iface
          Left err    -> do addErr (text err)
                            mzero
  where
  name = impModule i
