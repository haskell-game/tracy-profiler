{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module System.Tracy
  ( withProfiler
  , connected
  , waitConnected
  , waitConnected_

  , setThreadName
  , appInfo

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
  , plotConfig
  , PlotFormat(..)

#ifdef TRACY_FIBERS
  , fiberEnter
  , fiberLeave
#endif

  , Color
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import GHC.Exts (Addr#)

#if defined(TRACY_ENABLE)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Data.Text.Foreign qualified as Text
import Foreign (nullPtr)
import Foreign.C.ConstPtr (ConstPtr(..))
import GHC.Exts(Ptr(..))
import System.Timeout (timeout)
import System.Tracy.FFI qualified as FFI
#endif

import System.Tracy.FFI.Types (Color, PlotFormat(..))
import System.Tracy.Zone qualified as Zone

#if defined(TRACY_ENABLE) && defined(TRACY_MANUAL_LIFETIME)
import Control.Exception (bracket_)
#endif

{- | Start/stop profiler when TRACY_MANUAL_LIFETIME is enabled.
Otherwise does nothing.
-}
withProfiler :: IO a -> IO a
withProfiler action =
#if defined(TRACY_ENABLE) && defined(TRACY_MANUAL_LIFETIME)
  bracket_ FFI.startupProfiler FFI.shutdownProfiler action
#else
  action
#endif

{- | Check whether the profiler is connected.
-}
connected :: IO Bool
#if defined(TRACY_ENABLE)
connected = (/= 0) <$> FFI.connected
#else
connected = pure False
#endif

{- | Poll connection status repeatedly until the profiler is connected or the time is up.

Immediately returns False when Tracy is not enabled.
-}
waitConnected
  :: MonadIO m
  => Int       -- ^ Poll interval
  -> Maybe Int -- ^ Maximum waiting time
  -> m Bool
#if defined(TRACY_ENABLE)
waitConnected interval mtimeout =
  liftIO $
    case mtimeout of
      Nothing -> go
      Just t -> fromMaybe False <$> timeout t go
  where
    go = do
      conn <- connected
      if conn then
        pure True
      else
        threadDelay interval >> go
#else
waitConnected _interval _timeout = pure False -- XXX: ignore indefinite waiting
#endif

{- | Wait indefinitely until the profiler connects.

Useful for tests, benches, and other short-lifetime scripts.
-}
waitConnected_ :: IO ()
waitConnected_ = void $ waitConnected 100000 Nothing

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
frameMarkStart _name = pure ()
#endif

{-# INLINE frameMarkEnd #-}
frameMarkEnd :: MonadIO m => Addr# -> m ()
#if defined(TRACY_ENABLE)
frameMarkEnd name = liftIO $ FFI.emitFrameMarkEnd (ConstPtr $ Ptr name)
#else
frameMarkEnd _name = pure ()
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
messageC _color _txt = pure ()
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
plot _name _val = pure ()
#endif

{-# INLINE plotFloat #-}
plotFloat :: MonadIO m => Addr# -> Float -> m ()
#if defined(TRACY_ENABLE)
plotFloat name val = liftIO $
  FFI.emitPlotFloat (ConstPtr (Ptr name)) val
#else
plotFloat _name _val = pure ()
#endif

{-# INLINE plotInt #-}
plotInt :: MonadIO m => Addr# -> Int -> m ()
plotInt = plotIntegral

{-# INLINE plotIntegral #-}
#if defined(TRACY_ENABLE)
plotIntegral :: (Integral a, MonadIO m) => Addr# -> a -> m ()
plotIntegral name val = liftIO $
  FFI.emitPlotInt (ConstPtr (Ptr name)) (fromIntegral val)
#else
plotIntegral :: Monad m => Addr# -> a -> m ()
plotIntegral _name _val = pure ()
#endif

{-# INLINE plotConfig #-}
plotConfig
  :: MonadIO m
  => Addr#
  -> PlotFormat
  -> Bool -- ^ Step
  -> Bool -- ^ Fill
  -> Color
  -> m ()
#if defined(TRACY_ENABLE)
plotConfig name pf step fill col = liftIO $
  FFI.emitPlotConfig
    (ConstPtr (Ptr name))
    (fromIntegral $ fromEnum pf)
    (fromIntegral $ fromEnum step)
    (fromIntegral $ fromEnum fill)
    col
#else
plotConfig _name _typ _step _fill _col = pure ()
#endif

{- | Record additional information about the profiled application,
  which will be available in the trace description.

  This can include data such as the source repository revision, the
  application's environment (dev/prod), etc.
-}
{-# INLINE appInfo #-}
appInfo :: MonadIO m => Text -> m ()
#if defined(TRACY_ENABLE)
appInfo txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.emitMessageAppinfo (ConstPtr txtPtr) (fromIntegral txtSz)
#else
appInfo _txt = pure ()
#endif

#ifdef TRACY_FIBERS
{-# INLINE fiberEnter #-}
fiberEnter :: MonadIO m => Addr# -> m ()
#if defined(TRACY_ENABLE)
fiberEnter name = liftIO $
  FFI.fiberEnter (ConstPtr (Ptr name))
#else
fiberEnter _name = pure ()
#endif

{-# INLINE fiberLeave #-}
fiberLeave :: MonadIO m => m ()
#if defined(TRACY_ENABLE)
fiberLeave name val = liftIO $
  FFI.fiberLeave
#else
fiberLeave = pure ()
#endif
#endif
