module Mistral.Utils.Names where

import Mistral.ModuleSystem.Name ( Name )
import Mistral.Utils.Source ( Located(..) )

import qualified Data.Foldable as F
import qualified Data.Set as Set


-- Free Variables --------------------------------------------------------------

class FreeVars a where
  freeVars :: a -> Set.Set Name

instance FreeVars Name where
  freeVars = Set.singleton

instance FreeVars a => FreeVars [a] where
  freeVars = F.foldMap freeVars

instance FreeVars a => FreeVars (Maybe a) where
  freeVars = F.foldMap freeVars

instance FreeVars a => FreeVars (Located a) where
  freeVars = F.foldMap freeVars

instance (FreeVars a, FreeVars b) => FreeVars (a,b) where
  freeVars (a,b) = Set.union (freeVars a) (freeVars b)

instance (FreeVars a, FreeVars b, FreeVars c) => FreeVars (a,b,c) where
  freeVars (a,b,c) = Set.unions [freeVars a, freeVars b, freeVars c]


-- Bound Variables -------------------------------------------------------------

class BoundVars a where
  boundVars :: a -> Set.Set Name

instance BoundVars Name where
  boundVars = Set.singleton

instance BoundVars a => BoundVars [a] where
  boundVars = F.foldMap boundVars

instance BoundVars a => BoundVars (Maybe a) where
  boundVars = F.foldMap boundVars

instance BoundVars a => BoundVars (Located a) where
  boundVars = F.foldMap boundVars

instance (BoundVars a, BoundVars b) => BoundVars (a,b) where
  boundVars (a,b) = Set.union (boundVars a) (boundVars b)
