{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module System.Tracy
  ( withProfiler
  , connected

  , setThreadName

  , Zone.withSrcLoc_

  , frameMark_

  , messageL
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign (nullPtr)
import Foreign.C.ConstPtr (ConstPtr(..))
import GHC.Exts (Ptr(..), Addr#)

import System.Tracy.FFI qualified as FFI
import System.Tracy.Zone qualified as Zone

#if defined(TRACY_ENABLE) && defined(TRACY_MANUAL_LIFETIME)
import Control.Exception (bracket_)
#endif

withProfiler :: IO a -> IO a
withProfiler action =
#if defined(TRACY_ENABLE) && defined(TRACY_MANUAL_LIFETIME)
  bracket_ FFI.startupProfiler FFI.shutdownProfiler action
#else
  action
#endif

connected :: IO Bool
connected = (/= 0) <$> FFI.connected

setThreadName :: Addr# -> IO ()
#if defined(TRACY_ENABLE)
setThreadName name = FFI.setThreadName (ConstPtr (Ptr name))
#else
setThreadName _name = pure ()
#endif

{-# INLINE frameMark_ #-}
frameMark_ :: MonadIO m => m ()
#if defined(TRACY_ENABLE)
frameMark_ = liftIO $ FFI.emitFrameMark (ConstPtr nullPtr)
#else
frameMark_ = pure ()
#endif

{-# INLINE messageL #-}
messageL :: MonadIO m => Addr# -> m ()
#if defined(TRACY_ENABLE)
messageL txt = liftIO $ FFI.emitMessageL (ConstPtr (Ptr txt)) 0
#else
messageL _txt = pure ()
#endif
