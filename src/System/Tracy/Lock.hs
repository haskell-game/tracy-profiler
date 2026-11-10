{-# LANGUAGE CPP #-}

module System.Tracy.Lock
  ( -- * Lifetime
    Lock(..)
  , announce
  , terminate

    -- * Locking
  , acquire
  , tryAcquire
  , release

    -- * Annotations
  , mark
  , customName
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)

#ifdef TRACY_ENABLE
import Control.Monad (when)
import Data.Text.Foreign qualified as Text
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.Marshal.Utils (fromBool)

import System.Tracy.FFI qualified as FFI
#else
import Foreign.Ptr (nullPtr)
#endif

import System.Tracy.FFI.Types qualified as FFI
import System.Tracy.SourceLocation (SourceLocation(..))

newtype Lock = Lock FFI.TracyCLockCtx

{-# INLINE announce #-}
announce :: MonadIO m => SourceLocation -> m Lock
#ifdef TRACY_ENABLE
announce (SourceLocation location) = liftIO $ Lock <$> FFI.announceLockableCtx location
#else
announce _location = pure . Lock $ FFI.TracyCLockCtx nullPtr
#endif

{-# INLINE terminate #-}
terminate :: MonadIO m => Lock -> m ()
#ifdef TRACY_ENABLE
terminate (Lock ctx) = liftIO $ FFI.terminateLockableCtx ctx
#else
terminate _lock = pure ()
#endif

{-# INLINE acquire #-}
#ifdef TRACY_ENABLE
acquire :: MonadIO m => Lock -> m a -> m a
acquire (Lock ctx) takeLock = do
  waitRecorded <- liftIO $ FFI.beforeLockLockableCtx ctx
  result <- takeLock
  when (waitRecorded /= 0) . liftIO $ FFI.afterLockLockableCtx ctx
  pure result
#else
acquire :: Lock -> a -> a
acquire _lock takeLock = takeLock
#endif

{-# INLINE tryAcquire #-}
#ifdef TRACY_ENABLE
tryAcquire :: MonadIO m => Lock -> m Bool -> m Bool
tryAcquire (Lock ctx) tryTakeLock = do
  acquired <- tryTakeLock
  liftIO . FFI.afterTryLockLockableCtx ctx $ fromBool acquired
  pure acquired
#else
tryAcquire :: Lock -> a -> a
tryAcquire _lock tryTakeLock = tryTakeLock
#endif

{-# INLINE release #-}
#ifdef TRACY_ENABLE
release :: MonadIO m => Lock -> m a -> m a
release (Lock ctx) releaseLock = do
  liftIO $ FFI.beforeUnlockLockableCtx ctx
  releaseLock
#else
release :: Lock -> a -> a
release _lock releaseLock = releaseLock
#endif

{-# INLINE mark #-}
mark :: MonadIO m => Lock -> SourceLocation -> m ()
#ifdef TRACY_ENABLE
mark (Lock ctx) (SourceLocation location) = liftIO $ FFI.markLockableCtx ctx location
#else
mark _lock _location = pure ()
#endif

{-# INLINE customName #-}
customName :: MonadIO m => Lock -> Text -> m ()
#ifdef TRACY_ENABLE
customName (Lock ctx) txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.customNameLockableCtx ctx (ConstPtr txtPtr) (FFI.clampTextBytes txtSz)
#else
customName _lock _txt = pure ()
#endif
