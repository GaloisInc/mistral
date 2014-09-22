module Mistral.Utils.Lens where

import MonadLib
import Control.Applicative
import Data.Monoid

type LensLike f s t a b = (a -> f b) -> s -> f t

over            :: LensLike Id s t a b -> (a -> b) -> s -> t
over l f x       = runId (l (return.f) x)

set             :: LensLike Id s t a b -> b -> s -> t
set l a          = over l (const a)

view            :: LensLike (Const a) s s a a -> s -> a
view l x         = getConst (l Const x)

toListOf        :: LensLike (Const (Endo [a])) s s a a -> s -> [a]
toListOf l x     = getConst (l (\a -> Const (Endo (\b -> a:b))) x) `appEndo` []
