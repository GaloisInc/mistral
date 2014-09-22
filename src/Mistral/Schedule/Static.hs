{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

-- | This module handles the compile-time interpretation of the `main` schedule.
module Mistral.Schedule.Static (
    staticSchedule
  ) where

import Mistral.Driver
import Mistral.ModuleSystem.Export
import Mistral.ModuleSystem.Name
import Mistral.ModuleSystem.Interface
import Mistral.ModuleSystem.Prelude ( prelude )
import Mistral.Schedule.Interp
import Mistral.Schedule.Monad
import Mistral.Schedule.Value
import Mistral.TypeCheck.AST
import Mistral.Utils.PP
import Mistral.Utils.SCC

import           Control.Monad ( MonadPlus(..) )
import qualified Data.Foldable as Fold
import           Data.List ( find )
import           Data.Monoid


-- | Compute the static schedule of a module, given its dependencies.  The
-- module must contain a `main` schedule, to use as the entry point.
staticSchedule :: Module -> [Module] -> Driver Program
staticSchedule m deps = phase "schedule" $
                        runSchedule m deps $
                        withInterpEnv (modulesEnv (deps ++ [m])) $
  do main <- findMain m

     -- expand out the static topology, and assign tasks to nodes.
     net <- buildSchedule (bBody main)

     -- link together all modules, and write out the schedule to the final
     -- program.
     let prog = linkProgram (modName m) net (m:deps)
     traceMsg (pp prog)

     return prog

-- | Produce a static scheduling of tasks to nodes, or fail trying.
buildSchedule :: Expr -> Schedule SNetwork
buildSchedule e =
  do env <- getInterpEnv
     return (interpSchedule env e)

-- | Link together then scheduled network and all modules involved.
linkProgram :: Name -> SNetwork -> [Module] -> Program
linkProgram name net mods =
  Program { progName  = name
          , progNodes = map mkNode (snNodes net)
          , progLinks = snLinks net
          , progBinds = Fold.foldMap modBinds mods
          , progDatas = ifaceDatas prelude `mappend` Fold.foldMap modDatas mods }

mkNode :: SNode -> Named Node
mkNode sn = Named (snName sn) $
  Node { nSpec  = snSpec sn
       , nType  = snType sn
       , nTags  = map (ELit . LAtom) (snTags sn)
       , nTasks = map mkTask (snTasks sn) }

mkTask :: STask -> Named Task
mkTask st = Named (stName st) (stTask st)


-- Binding Interaction ---------------------------------------------------------

-- | Find a binding in a module.
findBind :: Name -> Module -> Schedule Decl
findBind n m = case find isBind (concatMap groupElems (modBinds m)) of
  Just b  -> return b
  Nothing -> do addErr $ fsep [ text "Unable to find binding" , backquotes (pp n)
                              , text "in module", backquotes (pp (modName m)) ]
                mzero
  where
  isBind b = bName b == n


-- | Discover the main schedule in a module.  Also, guard that it has no
-- closure, no parameters, and is public.
findMain :: Module -> Schedule Decl
findMain m =
  do b <- findBind mainSym m
     check (null (bCParams b))   "Non-empty closure on `main` schedule" $
      check (null (bParams b))    "Non-empty parameter list on `main` schedule" $
      check (bExport b == Public) "`main` schedule is not exported" $
      return b
  where
  check b msg k | b         = k
                | otherwise = do addErr (text msg)
                                 mzero

  ns      = modNamespace (modName m)
  mainSym = mkGlobal ns "main"
