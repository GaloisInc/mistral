module Mistral.TypeCheck (
    checkModule
  , genIface
  ) where

import           Mistral.Driver ( Driver, failErrs, phase, traceMsg )
import           Mistral.ModuleSystem.Interface ( IfaceTrie )
import qualified Mistral.Parser.AST as P
import           Mistral.TypeCheck.AST ( Module(modDeps) )
import           Mistral.TypeCheck.Infer ( tcModule )
import           Mistral.TypeCheck.Interface ( genIface )
import           Mistral.TypeCheck.Monad ( runTC )
import           Mistral.Utils.PP ( text, ($$), pp, nest, vcat )


-- | Typecheck a module, producing
checkModule :: IfaceTrie -> P.Module -> Driver Module
checkModule ifaces m = phase "tc" $
  do m' <- failErrs (runTC ifaces (tcModule m))
     traceMsg (text "" $$ pp m')
     traceMsg $ text "module dependencies"
             $$ nest 2 (vcat (map pp (modDeps m')))
     return m'
