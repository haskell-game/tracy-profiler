{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

{-| Tracing memory allocations

https://github.com/wolfpld/tracy/releases/download/v0.15.0/tracy.pdf

Events have to reach Tracy in the order the allocations and frees really happened.
If several threads allocate at once, hold a lock across each allocation or free and its report.
Otherwise a freed address can be handed out again and reported before its free is, which fails the capture.
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
  => Ptr a -- ^ address to register
  -> Int   -- ^ size
  -> m ()
#ifdef TRACY_ENABLE
alloc ptr size =
  liftIO $ FFI.emitMemoryAlloc (ConstPtr $ castPtr ptr) (fromIntegral size)
#else
alloc _ptr _size = pure ()
#endif

{-# INLINE free #-}
free
  :: MonadIO m
  => Ptr a -- ^ address that was registered
  -> m ()
#ifdef TRACY_ENABLE
free ptr =
  liftIO $ FFI.emitMemoryFree (ConstPtr $ castPtr ptr)
#else
free _ptr = pure ()
#endif

{-# INLINE allocNamed #-}
allocNamed
  :: MonadIO m
  => Ptr a -- ^ address to register
  -> Int   -- ^ size
  -> Addr# -- ^ memory pool name
  -> m ()
#ifdef TRACY_ENABLE
allocNamed ptr size name =
  liftIO $ FFI.emitMemoryAllocNamed (ConstPtr $ castPtr ptr) (fromIntegral size) (ConstPtr $ Ptr name)
#else
allocNamed _ptr _size _name = pure ()
#endif

{-# INLINE freeNamed #-}
freeNamed
  :: MonadIO m
  => Ptr a -- ^ address that was registered
  -> Addr# -- ^ memory pool name
  -> m ()
#ifdef TRACY_ENABLE
freeNamed ptr name =
  liftIO $ FFI.emitMemoryFreeNamed (ConstPtr $ castPtr ptr) (ConstPtr $ Ptr name)
#else
freeNamed _ptr _name = pure ()
#endif
