{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

{-| Tracing memory allocations

https://github.com/wolfpld/tracy/blob/master/manual/tracy.md#memory-profiling-memoryprofiling
-}

module System.Tracy.Memory
  ( -- * Generic allocation tracing
    alloc
  , free
    -- * Memory pools
  , allocNamed
  , freeNamed
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Ptr (Ptr)
import GHC.Exts (Addr#)

#ifdef TRACY_ENABLE
import GHC.Exts (Ptr(..))
import Foreign.Ptr (castPtr)
import Foreign.C.ConstPtr (ConstPtr(..))

import System.Tracy.FFI qualified as FFI
#endif

{-# INLINE alloc #-}
alloc
  :: MonadIO m
  => Bool  -- ^ secure
  -> Ptr a -- ^ address to register
  -> Int   -- ^ size
  -> m ()
#ifdef TRACY_ENABLE
alloc secure ptr size =
  liftIO $ FFI.emitMemoryAlloc (ConstPtr $ castPtr ptr) (fromIntegral size) (fromIntegral $ fromEnum secure)
#else
alloc _secure _ptr _size = pure ()
#endif

{-# INLINE free #-}
free
  :: MonadIO m
  => Bool  -- ^ secure
  -> Ptr a -- ^ address that was registered
  -> m ()
#ifdef TRACY_ENABLE
free secure ptr =
  liftIO $ FFI.emitMemoryFree (ConstPtr $ castPtr ptr) (fromIntegral $ fromEnum secure)
#else
free _secure _ptr = pure ()
#endif

{-# INLINE allocNamed #-}
allocNamed
  :: MonadIO m
  => Bool  -- ^ secure
  -> Ptr a -- ^ address to register
  -> Int   -- ^ size
  -> Addr# -- ^ memory pool name
  -> m ()
#ifdef TRACY_ENABLE
allocNamed secure ptr size name =
  liftIO $ FFI.emitMemoryAllocNamed (ConstPtr $ castPtr ptr) (fromIntegral size) (fromIntegral $ fromEnum secure) (ConstPtr $ Ptr name)
#else
allocNamed _secure _ptr _size _name = pure ()
#endif

{-# INLINE freeNamed #-}
freeNamed
  :: MonadIO m
  => Bool  -- ^ secure
  -> Ptr a -- ^ address that was registered
  -> Addr# -- ^ memory pool name
  -> m ()
#ifdef TRACY_ENABLE
freeNamed secure ptr name =
  liftIO $ FFI.emitMemoryFreeNamed (ConstPtr $ castPtr ptr) (fromIntegral $ fromEnum secure) (ConstPtr $ Ptr name)
#else
freeNamed _secure _ptr _name = pure ()
#endif
