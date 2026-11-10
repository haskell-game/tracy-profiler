{-# LANGUAGE CPP #-}

module System.Tracy.SharedLock
  ( -- * Lifetime
    SharedLock(..)
  , announce
  , terminate

    -- * Exclusive locking
  , acquire
  , tryAcquire
  , release

    -- * Shared locking
  , acquireShared
  , tryAcquireShared
  , releaseShared

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

newtype SharedLock = SharedLock FFI.TracyCSharedLockCtx

{-# INLINE announce #-}
announce :: MonadIO m => SourceLocation -> m SharedLock
#ifdef TRACY_ENABLE
announce (SourceLocation location) = liftIO $ SharedLock <$> FFI.announceSharedLockableCtx location
#else
announce _location = pure . SharedLock $ FFI.TracyCSharedLockCtx nullPtr
#endif

{-# INLINE terminate #-}
terminate :: MonadIO m => SharedLock -> m ()
#ifdef TRACY_ENABLE
terminate (SharedLock ctx) = liftIO $ FFI.terminateSharedLockableCtx ctx
#else
terminate _lock = pure ()
#endif

{-# INLINE acquire #-}
#ifdef TRACY_ENABLE
acquire :: MonadIO m => SharedLock -> m a -> m a
acquire (SharedLock ctx) takeLock = do
  waitRecorded <- liftIO $ FFI.beforeLockSharedLockableCtx ctx
  result <- takeLock
  when (waitRecorded /= 0) . liftIO $ FFI.afterLockSharedLockableCtx ctx
  pure result
#else
acquire :: SharedLock -> a -> a
acquire _lock takeLock = takeLock
#endif

{-# INLINE tryAcquire #-}
#ifdef TRACY_ENABLE
tryAcquire :: MonadIO m => SharedLock -> m Bool -> m Bool
tryAcquire (SharedLock ctx) tryTakeLock = do
  acquired <- tryTakeLock
  liftIO . FFI.afterTryLockSharedLockableCtx ctx $ fromBool acquired
  pure acquired
#else
tryAcquire :: SharedLock -> a -> a
tryAcquire _lock tryTakeLock = tryTakeLock
#endif

{-# INLINE release #-}
#ifdef TRACY_ENABLE
release :: MonadIO m => SharedLock -> m a -> m a
release (SharedLock ctx) releaseLock = do
  liftIO $ FFI.beforeUnlockSharedLockableCtx ctx
  releaseLock
#else
release :: SharedLock -> a -> a
release _lock releaseLock = releaseLock
#endif

{-# INLINE acquireShared #-}
#ifdef TRACY_ENABLE
acquireShared :: MonadIO m => SharedLock -> m a -> m a
acquireShared (SharedLock ctx) takeLock = do
  waitRecorded <- liftIO $ FFI.beforeLockSharedSharedLockableCtx ctx
  result <- takeLock
  when (waitRecorded /= 0) . liftIO $ FFI.afterLockSharedSharedLockableCtx ctx
  pure result
#else
acquireShared :: SharedLock -> a -> a
acquireShared _lock takeLock = takeLock
#endif

{-# INLINE tryAcquireShared #-}
#ifdef TRACY_ENABLE
tryAcquireShared :: MonadIO m => SharedLock -> m Bool -> m Bool
tryAcquireShared (SharedLock ctx) tryTakeLock = do
  acquired <- tryTakeLock
  liftIO . FFI.afterTryLockSharedSharedLockableCtx ctx $ fromBool acquired
  pure acquired
#else
tryAcquireShared :: SharedLock -> a -> a
tryAcquireShared _lock tryTakeLock = tryTakeLock
#endif

{-# INLINE releaseShared #-}
#ifdef TRACY_ENABLE
releaseShared :: MonadIO m => SharedLock -> m a -> m a
releaseShared (SharedLock ctx) releaseLock = do
  liftIO $ FFI.beforeUnlockSharedSharedLockableCtx ctx
  releaseLock
#else
releaseShared :: SharedLock -> a -> a
releaseShared _lock releaseLock = releaseLock
#endif

{-# INLINE mark #-}
mark :: MonadIO m => SharedLock -> SourceLocation -> m ()
#ifdef TRACY_ENABLE
mark (SharedLock ctx) (SourceLocation location) = liftIO $ FFI.markSharedLockableCtx ctx location
#else
mark _lock _location = pure ()
#endif

{-# INLINE customName #-}
customName :: MonadIO m => SharedLock -> Text -> m ()
#ifdef TRACY_ENABLE
customName (SharedLock ctx) txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.customNameSharedLockableCtx ctx (ConstPtr txtPtr) (FFI.clampTextBytes txtSz)
#else
customName _lock _txt = pure ()
#endif
