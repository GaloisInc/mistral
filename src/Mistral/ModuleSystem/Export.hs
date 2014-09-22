{-# LANGUAGE DeriveGeneric #-}

module Mistral.ModuleSystem.Export where

import Mistral.Utils.PP

import GHC.Generics
import Data.Serialize ( Serialize )


data Export = Public | Private
              deriving (Show,Eq,Generic)

instance Serialize Export

instance PP Export where
  ppr e = text $ case e of
    Public  -> "public"
    Private -> "private"
