{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module System.Tracy.Memory
  ( alloc
  , free
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

alloc :: MonadIO m => Bool -> Ptr a -> Int -> m ()
#ifdef TRACY_ENABLE
alloc secure ptr size =
  liftIO $ FFI.emitMemoryAlloc (ConstPtr $ castPtr ptr) (fromIntegral size) (fromIntegral $ fromEnum secure)
#else
alloc _secure _ptr _size = pure ()
#endif

free :: MonadIO m => Bool -> Ptr a -> m ()
#ifdef TRACY_ENABLE
free secure ptr =
  liftIO $ FFI.emitMemoryFree (ConstPtr $ castPtr ptr) (fromIntegral $ fromEnum secure)
#else
free _secure _ptr = pure ()
#endif

allocNamed :: MonadIO m => Bool -> Ptr a -> Int -> Addr# -> m ()
#ifdef TRACY_ENABLE
allocNamed secure ptr size name =
  liftIO $ FFI.emitMemoryAllocNamed (ConstPtr $ castPtr ptr) (fromIntegral size) (fromIntegral $ fromEnum secure) (ConstPtr $ Ptr name)
#else
allocNamed _secure _ptr _size _name = pure ()
#endif

freeNamed :: MonadIO m => Bool -> Ptr a -> Addr# -> m ()
#ifdef TRACY_ENABLE
freeNamed secure ptr name =
  liftIO $ FFI.emitMemoryFreeNamed (ConstPtr $ castPtr ptr) (fromIntegral $ fromEnum secure) (ConstPtr $ Ptr name)
#else
freeNamed _secure _ptr _name = pure ()
#endif
