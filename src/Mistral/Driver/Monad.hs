{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Mistral.Driver.Monad (
    Driver(), DriverM
  , runDriver, runDriverOpts
  , io
  , ppM
  , try

    -- * Debug Messages
  , phase
  , traceMsg

    -- * Module Loading
  , RWBytes(..), rwBytesFS
  , getBytes
  , saveBytes

    -- * Messages
  , tryMessages
  , tryMapM
  , collectMessages
  , failErrs

    -- ** Errors
  , Error(..)
  , addErr, addErrs
  , addErrLoc
  , addErrAt, addErrsAt
  , mkError

    -- ** Warnings
  , Warning(..)
  , addWarn, addWarns
  , addWarnLoc
  , addWarnAt, addWarnsAt
  ) where

import Mistral.Utils.PP
import Mistral.Utils.Source

import           Control.Applicative ( Applicative(), Alternative(..) )
import qualified Control.Exception as X
import           Control.Monad ( MonadPlus(..), guard )
import           Control.Monad.Fix ( MonadFix )
import qualified Data.ByteString.Lazy as L
import           Data.IORef
                 ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef )
import           Data.Maybe ( catMaybes )
import           Data.Typeable ( Typeable )
import           MonadLib ( ReaderT, ask, RunM(..), BaseM(..) )
import           Text.PrettyPrint.HughesPJ ( Doc )


-- Utils -----------------------------------------------------------------------

atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f =
  do b <- atomicModifyIORef ref (\x -> let (a,b) = f x
                                        in (a, a `seq` b))
     b `seq` return b

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f =
  do a <- readIORef ref
     let a' = f a
     a' `seq` writeIORef ref a'


-- Environment -----------------------------------------------------------------


data RO = RO { roErrors      :: IORef [Error]
             , roWarns       :: IORef [Warning]
             , roPPEnv       :: PPEnv
             , roTracePhases :: [String]
             , roLogger      :: IORef (Doc -> IO ())
             , roRWBytes     :: RWBytes
             }

initialRO :: IO RO
initialRO  = do
  errs   <- newIORef []
  warns  <- newIORef []
  logRef <- newIORef ignoreMsg
  return RO { roErrors      = errs
            , roWarns       = warns
            , roPPEnv       = defaultPPEnv
            , roTracePhases = []
            , roLogger      = logRef
            , roRWBytes     = rwBytesFS
            }

data DriverFail = DriverFail (Maybe String)
                  deriving (Show,Typeable)

instance X.Exception DriverFail


type DriverM drv = ( Functor drv, Applicative drv, Monad drv, MonadPlus drv
                   , BaseM drv Driver)

newtype Driver a = Driver { getDriver :: ReaderT RO IO a
                          } deriving (Functor,Applicative,MonadFix)


instance Monad Driver where
  {-# INLINE return #-}
  return x = Driver (return x)

  {-# INLINE (>>=) #-}
  m >>= f = Driver (getDriver m >>= getDriver . f)

  {-# INLINE fail #-}
  fail msg = addErr (text msg) >> mzero -- io (X.throwIO (DriverFail (Just msg)))

instance MonadPlus Driver where
  {-# INLINE mzero #-}
  mzero     = io (X.throwIO (DriverFail Nothing))

  {-# INLINE mplus #-}
  mplus a b = Driver $
    do ro <- ask
       let run m = runM (getDriver m) ro
       inBase (run a `X.catch` \ DriverFail{} -> run b)

instance Alternative Driver where
    empty = mzero
    (<|>) = mplus

instance BaseM Driver Driver where
  {-# INLINE inBase #-}
  inBase = id

instance RunM Driver a (Driver a) where
  {-# INLINE runM #-}
  runM = id


runDriver :: Driver a -> IO a
runDriver  = runDriverOpts defaultPPEnv [] rwBytesFS

runDriverOpts :: PPEnv -> [String] -> RWBytes -> Driver a -> IO a
runDriverOpts pe tracePhases rwBytes m = do
  ro <- initialRO
  runM (getDriver m) ro { roPPEnv       = pe
                        , roTracePhases = tracePhases
                        , roRWBytes     = rwBytes
                        }

io :: (BaseM drv Driver) => IO a -> drv a
io m = inBase (Driver (inBase m))

ppM :: (BaseM drv Driver, PP a) => a -> drv Doc
ppM a = inBase $ Driver $
  do ro <- ask
     return (runPPM (roPPEnv ro) (pp a))


-- Interface Loading -----------------------------------------------------------

data RWBytes = RWBytes { rwGetBytes  :: FilePath -> IO (Maybe L.ByteString)
                       , rwSaveBytes :: FilePath -> L.ByteString -> IO Bool
                       }

rwBytesFS :: RWBytes
rwBytesFS  = RWBytes { rwGetBytes = get, rwSaveBytes = save }
  where
  get path = fmap Just (L.readFile path) `X.catch` handler
    where
    handler :: X.IOException -> IO (Maybe L.ByteString)
    handler _ = return Nothing

  save path bytes = X.handle handler $
    do () <- L.writeFile path bytes
       return True
    where
    handler :: X.IOException -> IO Bool
    handler _ = return False



-- | Attempt to load the interface associated with the (mangled) module name.
getBytes :: BaseM drv Driver => FilePath -> drv (Maybe L.ByteString)
getBytes path = inBase $
  do ro <- Driver ask
     io (rwGetBytes (roRWBytes ro) path)

-- | Returns True, when the bytes have been successfully saved.
saveBytes :: BaseM drv Driver => FilePath -> L.ByteString -> drv Bool
saveBytes path bytes = inBase $
  do ro <- Driver ask
     io (rwSaveBytes (roRWBytes ro) path bytes)


-- Phase Logging ---------------------------------------------------------------

-- | Set the phase for a driver action.
phase :: BaseM drv Driver => String -> drv a -> drv a
phase name body =
  do -- install the new message logger
     logger <- inBase $ Driver $
       do ro     <- ask
          logger <- inBase (readIORef (roLogger ro))
          let logger' | name `elem` roTracePhases ro = writeMsg
                      | otherwise                    = ignoreMsg
          inBase (writeIORef (roLogger ro) logger')
          return logger

     -- run the body
     banner ("begin [" ++ name ++ "]")
     a <- body
     banner ("end [" ++ name ++ "]")

     -- restore the original logger
     inBase $ Driver $
       do ro <- ask
          inBase (writeIORef (roLogger ro) logger)

     return a

banner :: BaseM drv Driver => String -> drv ()
banner msg = traceMsg (text paddedMsg)
  where
  paddedMsg = take 80 (unwords [ "--", msg, "--%<" ++ repeat '-' ])

writeMsg :: Doc -> IO ()
writeMsg  = print

ignoreMsg :: Doc -> IO ()
ignoreMsg _ = return ()

-- | Trace a message if the current phase allows it.
traceMsg :: BaseM drv Driver => PPDoc -> drv ()
traceMsg msg =
  do ro  <- inBase (Driver ask)
     doc <- ppM msg
     io $ do write <- readIORef (roLogger ro)
             write doc


-- Errors and Warnings ---------------------------------------------------------

-- | Try to run a computation.
try :: MonadPlus drv => drv a -> drv (Maybe a)
try m = run `mplus` return Nothing
  where
  run = do a <- m
           return (Just a)

-- | Run a sub-computation, 
tryMessages :: (BaseM drv Driver, MonadPlus drv)
            => drv a -> drv ([Error],[Warning],Maybe a)
tryMessages m = collectMessages (try m)

-- | Like mapM, but filters out results that fail.
tryMapM :: MonadPlus drv => (a -> drv b) -> [a] -> drv [b]
tryMapM f as = 
  do ms <- mapM (try . f) as
     return (catMaybes ms)


-- | Collect the messages emitted by an operation.
collectMessages :: BaseM drv Driver => drv a -> drv ([Error],[Warning],a)
collectMessages m =
  do (es0,ws0) <- inBase (swapMessages [] [])
     a <- m
     (es,ws) <- inBase (swapMessages es0 ws0)
     return (es,ws,a)
  where
  swapMessages es0 ws0 = Driver $
    do ro  <- ask
       es  <- inBase (atomicModifyIORef' (roErrors ro) (\ es -> (es0, es)))
       ws  <- inBase (atomicModifyIORef' (roWarns  ro) (\ ws -> (ws0, ws)))
       return (es,ws)

-- | Fail when the sub-computation produces errors.
failErrs :: (BaseM drv Driver, MonadPlus drv) => drv a -> drv a
failErrs m =
  do (es,ws,a) <- collectMessages m
     emitErrs es
     emitWarns ws
     guard (null es)
     return a



data Error = Error PPDoc Source
             deriving (Show)

-- | Construct an error, given a location and pretty-printable thing.
mkError :: PP e => e -> Source -> Error
mkError e = Error (pp e)

-- | Collect a list of errors.
emitErrs :: BaseM drv Driver => [Error] -> drv ()
emitErrs msgs = inBase $ Driver $
  do ro <- ask
     inBase (modifyIORef' (roErrors ro) (++ msgs))

-- | Add a single error with no associated location.
addErr :: (BaseM drv Driver, PP e) => e -> drv ()
addErr msg = addErrsAt [msg] Unknown

-- | Add a single error message, with a location.
addErrAt :: (BaseM drv Driver, PP e) => e -> Source -> drv ()
addErrAt msg src = addErrsAt [msg] src

-- | Add an error message for something that provides its own location.
addErrLoc :: (BaseM drv Driver, HasSource e, PP e) => e -> drv ()
addErrLoc msg = addErrsAt [msg] (getSource msg)

-- | Add a list of error messages, all with no location.
addErrs :: (BaseM drv Driver, PP e) => [e] -> drv ()
addErrs msgs = addErrsAt msgs Unknown

-- | Add a list of error messages, all from the same location.
addErrsAt :: (BaseM drv Driver, PP e) => [e] -> Source -> drv ()
addErrsAt msgs src = emitErrs [ mkError m src | m <- msgs ]


data Warning = Warning PPDoc Source
               deriving (Show)

-- | Construct a warning from a pretty-printable thing and a location.
mkWarning :: PP w => w -> Source -> Warning
mkWarning w = Warning (pp w)

-- | Emit a list of warnings all at once.
emitWarns :: BaseM drv Driver => [Warning] -> drv ()
emitWarns msgs = inBase $ Driver $
  do ro <- ask
     inBase (modifyIORef' (roWarns ro) (++ msgs))

-- | Add a single warning, with no location information.
addWarn :: (BaseM drv Driver, PP w) => w -> drv ()
addWarn msg = addWarnsAt [msg] Unknown

-- | Add a single warning, with location information.
addWarnAt :: (BaseM drv Driver, PP w) => w -> Source -> drv ()
addWarnAt msg = addWarnsAt [msg]

-- | Add a warning for something that provides its own location information.
addWarnLoc :: (BaseM drv Driver, HasSource w, PP w) => w -> drv ()
addWarnLoc msg = addWarnsAt [msg] (getSource msg)

-- | Add a list of warnings, all with no associated location information.
addWarns :: (BaseM drv Driver, PP w) => [w] -> drv ()
addWarns msgs = addWarnsAt msgs Unknown

-- | Add a list of warnings, all from the same location.
addWarnsAt :: (BaseM drv Driver, PP w) => [w] -> Source -> drv ()
addWarnsAt msgs src = emitWarns [ mkWarning w src | w <- msgs ]


instance PP Error where
  ppr (Error msg src) = char ' '
                     $$ text "[error]" <+> pp src
                     $$ nest 2 msg

instance PP Warning where
  ppr (Warning msg src) = char ' '
                       $$ text "[warning]" <+> pp src
                       $$ nest 2 msg

instance HasSource Error where
  getSource (Error _ src) = src

instance HasSource Warning where
  getSource (Warning _ src) = src
