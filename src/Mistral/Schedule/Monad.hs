{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mistral.Schedule.Monad where

import Mistral.Driver
import Mistral.Schedule.Value
import Mistral.TypeCheck.AST

import Control.Applicative ( Applicative, Alternative )
import Control.Monad ( MonadPlus )
import Data.Monoid ( Monoid(..) )
import MonadLib ( BaseM(..), ReaderT, ask, local, runM )


-- Schedule Monad --------------------------------------------------------------

newtype Schedule a = Schedule { getSchedule :: ReaderT RO Driver a
                              } deriving (Functor,Applicative,Monad,MonadPlus,Alternative)

instance BaseM Schedule Driver where
  inBase m = Schedule (inBase m)

runSchedule :: Module -> [Module] -> Schedule a -> Driver a
runSchedule m deps body = failErrs (runM (getSchedule body) ro)
  where
  ro = RO { roMainMod = m
          , roDeps    = deps
          , roEnv     = mempty }

data RO = RO { roMainMod :: Module
             , roDeps    :: [Module]
             , roEnv     :: Env }

withInterpEnv :: Env -> Schedule a -> Schedule a
withInterpEnv env m = Schedule $
  do ro <- ask
     local ro { roEnv = env `mappend` roEnv ro } (getSchedule m)

getInterpEnv :: Schedule Env
getInterpEnv  = Schedule (roEnv `fmap` ask)
