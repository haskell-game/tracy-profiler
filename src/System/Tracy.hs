{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module System.Tracy
  ( withProfiler
  , connected

  , setThreadName

  , Zone.withSrcLoc_

  , frameMark
  , frameMark_
  , frameMarkStart
  , frameMarkEnd

  , message
  , messageC
  , messageL
  , messageLC

  , plot
  , plotFloat
  , plotInt
  , plotIntegral

  , Color
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign (nullPtr)
import Foreign.C.ConstPtr (ConstPtr(..))
import GHC.Exts (Ptr(..), Addr#)
import Data.Text (Text)
import Data.Text.Foreign qualified as Text

import System.Tracy.FFI qualified as FFI
import System.Tracy.FFI.Types (Color)
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

{-# INLINE frameMark #-}
frameMark :: MonadIO m => Addr# -> m ()
#if defined(TRACY_ENABLE)
frameMark name = liftIO $ FFI.emitFrameMark (ConstPtr $ Ptr name)
#else
frameMark _name = pure ()
#endif

{-# INLINE frameMarkStart #-}
frameMarkStart :: MonadIO m => Addr# -> m ()
#if defined(TRACY_ENABLE)
frameMarkStart name = liftIO $ FFI.emitFrameMarkStart (ConstPtr $ Ptr name)
#else
frameMarkStart = pure ()
#endif

{-# INLINE frameMarkEnd #-}
frameMarkEnd :: MonadIO m => Addr# -> m ()
#if defined(TRACY_ENABLE)
frameMarkEnd name = liftIO $ FFI.emitFrameMarkEnd (ConstPtr $ Ptr name)
#else
frameMarkEnd = pure ()
#endif

-- TODO: nicer wrapper for emitFrameImage

{-# INLINE message #-}
message :: MonadIO m => Text -> m ()
#if defined(TRACY_ENABLE)
message txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.emitMessage (ConstPtr txtPtr) (fromIntegral txtSz) 0
#else
message _txt = pure ()
#endif

{-# INLINE messageC #-}
messageC :: MonadIO m => Color -> Text -> m ()
#if defined(TRACY_ENABLE)
messageC color txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.emitMessageC (ConstPtr txtPtr) (fromIntegral txtSz) color 0
#else
messageC _txt = pure ()
#endif


{-# INLINE messageL #-}
messageL :: MonadIO m => Addr# -> m ()
#if defined(TRACY_ENABLE)
messageL txt = liftIO $
  FFI.emitMessageL (ConstPtr (Ptr txt)) 0
#else
messageL _txt = pure ()
#endif

{-# INLINE messageLC #-}
messageLC :: MonadIO m => Color -> Addr# -> m ()
#if defined(TRACY_ENABLE)
messageLC color txt = liftIO $
  FFI.emitMessageLC (ConstPtr (Ptr txt)) color 0
#else
messageLC _color _txt = pure ()
#endif

{-# INLINE plot #-}
plot :: MonadIO m => Addr# -> Double -> m ()
#if defined(TRACY_ENABLE)
plot name val = liftIO $
  FFI.emitPlot (ConstPtr (Ptr name)) val
#else
plot _name = pure ()
#endif

{-# INLINE plotFloat #-}
plotFloat :: MonadIO m => Addr# -> Float -> m ()
#if defined(TRACY_ENABLE)
plotFloat name val = liftIO $
  FFI.emitPlotFloat (ConstPtr (Ptr name)) val
#else
plotFloat _name = pure ()
#endif

{-# INLINE plotInt #-}
plotInt :: MonadIO m => Addr# -> Int -> m ()
plotInt = plotIntegral

{-# INLINE plotIntegral #-}
plotIntegral :: (Integral a, MonadIO m) => Addr# -> a -> m ()
#if defined(TRACY_ENABLE)
plotIntegral name val = liftIO $
  FFI.emitPlotInt (ConstPtr (Ptr name)) (fromIntegral val)
#else
plotIntegral _name = pure ()
#endif

-- TODO: emitPlotConfig

-- TODO: emitMessageAppinfo

-- TODO: fiberEnter
-- TODO: fiberLeave
