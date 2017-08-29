{-# LANGUAGE FlexibleContexts #-}

module Mistral.CodeGen.Link where

import Mistral.Schedule.Static ( staticSchedule )
import Mistral.Driver
import Mistral.ModuleSystem.Prelude ( prelude )
import Mistral.ModuleSystem.Interface ( Iface(..) )
import Mistral.TypeCheck.AST
import Mistral.Utils.PP

import qualified Data.Set as Set


-- | Link a module into a program.  The module is expected to have a `main`
-- schedule, which will be used as the entry point to the whole program.
link :: Module -> Driver Program
link m = phase "link" $
  do traceMsg (text "Linking module:" <+> pp (modName m))
     depMods <- loadDeps m
     staticSchedule m depMods

-- | Load all module dependencies.
loadDeps :: Module -> Driver [Module]
loadDeps m = failErrs (go Set.empty [] (modDeps m))
  where
  go loaded acc ns = case ns of

    n:rest
      | Set.member n loaded ->
        go loaded acc rest

        -- skip the prelude
      | n == ifaceModName prelude ->
        do traceMsg (text "Skipping:" <+> pp n)
           go (Set.insert n loaded) acc rest

      | otherwise ->
        do traceMsg (text "Loading dependency:" <+> pp n)
           e <- loadModule n
           case e of
             Right m' -> go (Set.insert n loaded) (m':acc) (modDeps m' ++ rest)
             Left err -> do addErr (text err)
                            go loaded acc rest

    [] -> return acc
