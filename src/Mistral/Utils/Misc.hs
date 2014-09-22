
module Mistral.Utils.Misc where

import Control.Monad (liftM)

-- | Jeez, why is this not in 'Control.Monad'? (Code taken from GHC
-- bug 2042)
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs  = liftM concat (mapM f xs)

