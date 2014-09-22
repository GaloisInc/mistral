module Mistral.Simple (
    -- * High-level interface
    compileFile
  , compileModule
  , linkProgram
    -- * Low-level interface (subject to change outside of PVP)
  , compile'
  , version
  ) where

import Mistral.CodeGen.DeadCode ( elimDeadCode )
import Mistral.CodeGen.LambdaLift ( lambdaLift )
import Mistral.CodeGen.Link ( link )
import Mistral.CodeGen.ResolveTags ( resolveTags )
import Mistral.CodeGen.Specialize ( specialize )
import Mistral.Driver ( Driver, io, phase, failErrs, addErr, traceMsg )
import Mistral.ModuleSystem ( moduleSystem, saveIface )
import Mistral.Parser ( parseModule )
import Mistral.TypeCheck ( checkModule )
import Mistral.TypeCheck.AST ( Module, Program, saveModule )
import Mistral.TypeCheck.Interface ( genIface )
import Mistral.Utils.PP ( text, ($$), pp )

import           Data.Version (Version)
import qualified Paths_mistral as P
import           Control.Monad ( when, unless )
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L

-- | Compile the contents of a file
compileFile :: FilePath -> Driver Module
compileFile path = phase "compile" $
  do bytes <- io (L.readFile path)
     compile' True (Just path) bytes

-- | Compile a text buffer, optionally with a path attached, but do
-- not write the output to disk.
compileModule :: Maybe FilePath -> L.Text -> Driver Module
compileModule mbPath bytes =
    phase "compile" (compile' False mbPath bytes)

-- | Parse, rename, typecheck and lambda-lift a module.  Then, write out its
-- interface and compiled object if the flag is set
compile' :: Bool -> Maybe FilePath -> L.Text -> Driver Module
compile' writeFiles mbPath bytes = failErrs $
  do (m,ifaces) <- moduleSystem =<< parseModule mbPath bytes
     cm        <- lambdaLift =<< checkModule ifaces m

     when writeFiles $
          do ifaceSaved <- saveIface (genIface cm)
             unless ifaceSaved (addErr (text "failed to write interface"))

             modSaved   <- saveModule cm
             unless modSaved (addErr (text "failed to write compiled module"))

     return cm

-- | Link, specialize and resolve tags, in preparation for packaging for
-- runtime, or generating code.
linkProgram :: Module -> Driver Program
linkProgram m = phase "linkProgram" $
  do prog <- resolveTags =<< elimDeadCode =<< specialize =<< link m
     traceMsg (text "Linked program:" $$ pp prog)
     return prog

version :: Version
version = P.version
