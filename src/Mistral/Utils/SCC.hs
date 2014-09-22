{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module Mistral.Utils.SCC ( Group(..), scc, groupElems, groupMap, groupMapM ) where

import Mistral.Utils.Names

import qualified Data.Foldable as F
import           Data.Graph ( stronglyConnComp, SCC(..) )
import qualified Data.Set as Set
import           Data.Serialize ( Serialize )
import qualified Data.Traversable as T
import           GHC.Generics ( Generic )


-- | Single, non-recursive elements, or a recursive group.
data Group a = NonRecursive a
             | Recursive [a]
               deriving (Generic,Show,Functor,F.Foldable,T.Traversable)

instance Serialize a => Serialize (Group a)

instance FreeVars a => FreeVars (Group a) where
  freeVars = F.foldMap freeVars

instance BoundVars a => BoundVars (Group a) where
  boundVars = F.foldMap boundVars

-- | Flatten a group.
groupElems :: Group a -> [a]
groupElems  = F.toList

-- | Map over a group
groupMap :: (a -> b) -> Group a -> Group b
groupMap  = fmap

-- | Map over a group in a monad
groupMapM :: Monad m => (a -> m b) -> Group a -> m (Group b)
groupMapM  = T.mapM

-- | Compute the strongly connected components of a list of things that define,
-- and reference names.
scc :: (BoundVars a, FreeVars a) => [a] -> [Group a]
scc as = map toGroup (stronglyConnComp graph)
  where
  graph = [ (a, n, map Just fvs)
          | a <- as
          , let fvs               = Set.toList (freeVars a)
                bvs               = Set.toList (boundVars a)
                names | null bvs  = [ Nothing ]
                      | otherwise = map Just bvs
          , n <- names
          ]

-- | Convert between SCC and Group.
toGroup :: SCC a -> Group a
toGroup c = case c of
  AcyclicSCC a -> NonRecursive a
  CyclicSCC as -> Recursive as

