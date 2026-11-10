{-# LANGUAGE CPP #-}

module System.Tracy.Gpu
  ( -- * Contexts
    Context(..)
  , GpuContextType(..)
  , newContext
  , newCalibratedContext
  , contextName

    -- * Scoped zones
  , withSrcLoc
  , withSourceLocation

    -- * Manual zones
  , Zone(..)
  , beginSrcLoc
  , beginSourceLocation
  , end

    -- * Timestamps
  , time
  , calibration
  , timeSync
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word8, Word16)

#ifdef TRACY_ENABLE
import Control.Exception (bracket)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString.Char8 qualified as ByteString
import Data.Text.Foreign qualified as Text
import Foreign.C.ConstPtr (ConstPtr(..))
import GHC.Stack (HasCallStack, SrcLoc(..), callStack, getCallStack, withFrozenCallStack)

import System.Tracy.FFI qualified as FFI
import System.Tracy.FFI.Types (gpuContextTypeCode, maxTextBytes)
import System.Tracy.Zone qualified as Zone
#endif

import System.Tracy.FFI.Types (Color, GpuContextType(..))
import System.Tracy.SourceLocation (SourceLocation(..))

newtype Context = Context Word8
  deriving (Eq, Ord, Show)

newtype Zone = Zone (Maybe Context)
  deriving (Eq, Ord, Show)

uncalibratedFlags :: Word8
uncalibratedFlags = 0

calibratedFlags :: Word8
calibratedFlags = 1

{-# INLINE newContext #-}
newContext :: MonadIO m => Context -> GpuContextType -> Float -> Int64 -> m ()
newContext = emitNewContext uncalibratedFlags

{-# INLINE newCalibratedContext #-}
newCalibratedContext :: MonadIO m => Context -> GpuContextType -> Float -> Int64 -> m ()
newCalibratedContext = emitNewContext calibratedFlags

{-# INLINE emitNewContext #-}
emitNewContext :: MonadIO m => Word8 -> Context -> GpuContextType -> Float -> Int64 -> m ()
#ifdef TRACY_ENABLE
emitNewContext flags (Context contextId) contextType period gpuTime = liftIO $
  FFI.emitGpuNewContextSerial gpuTime period contextId flags (gpuContextTypeCode contextType)
#else
emitNewContext _flags _context _contextType _period _gpuTime = pure ()
#endif

{-# INLINE contextName #-}
contextName :: MonadIO m => Context -> Text -> m ()
#ifdef TRACY_ENABLE
contextName (Context contextId) txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.emitGpuContextNameSerial contextId (ConstPtr txtPtr) (fromIntegral $ min maxTextBytes txtSz)
#else
contextName _context _txt = pure ()
#endif

{-# INLINE withSrcLoc #-}
{-# INLINE withSourceLocation #-}
#ifdef TRACY_ENABLE
withSrcLoc
  :: (HasCallStack, MonadUnliftIO m)
  => Context
  -> ByteString
  -> Color
  -> m Word16
  -> m Word16
  -> m a
  -> m a
withSrcLoc context function col writeBeginTimestamp =
  withZone (withFrozenCallStack (beginSrcLoc context function col writeBeginTimestamp))

withSourceLocation
  :: MonadUnliftIO m
  => Context
  -> SourceLocation
  -> m Word16
  -> m Word16
  -> m a
  -> m a
withSourceLocation context location writeBeginTimestamp =
  withZone (beginSourceLocation context location writeBeginTimestamp)

{-# INLINE withZone #-}
withZone :: MonadUnliftIO m => m Zone -> m Word16 -> m a -> m a
withZone begin writeEndTimestamp action =
  withRunInIO \run ->
    bracket
      (run begin)
      (\zone -> run $ end zone writeEndTimestamp)
      (\_zone -> run action)
#else
withSrcLoc :: Context -> ByteString -> Color -> m Word16 -> m Word16 -> m a -> m a
withSrcLoc _context _function _col _writeBeginTimestamp _writeEndTimestamp action = action

withSourceLocation :: Context -> SourceLocation -> m Word16 -> m Word16 -> m a -> m a
withSourceLocation _context _location _writeBeginTimestamp _writeEndTimestamp action = action
#endif

{-# INLINE beginSrcLoc #-}
#ifdef TRACY_ENABLE
beginSrcLoc :: (HasCallStack, MonadIO m) => Context -> ByteString -> Color -> m Word16 -> m Zone
beginSrcLoc context@(Context contextId) function col writeTimestamp =
  case getCallStack callStack of
    (_called, SrcLoc{srcLocFile, srcLocStartLine}) : _callers ->
      beginWith context writeTimestamp \queryId -> do
        srcloc <-
          Zone.allocSrcloc
            (fromIntegral srcLocStartLine)
            (ByteString.pack srcLocFile)
            function
            Nothing
            col
        FFI.emitGpuZoneBeginAllocSerial srcloc queryId contextId
    [] ->
      error "beginSrcLoc ought to have HasCallStack in context"
#else
beginSrcLoc :: MonadIO m => Context -> ByteString -> Color -> m Word16 -> m Zone
beginSrcLoc _context _function _col _writeTimestamp = pure $ Zone Nothing
#endif

{-# INLINE beginSourceLocation #-}
beginSourceLocation :: MonadIO m => Context -> SourceLocation -> m Word16 -> m Zone
#ifdef TRACY_ENABLE
beginSourceLocation context@(Context contextId) (SourceLocation locationData) writeTimestamp =
  beginWith context writeTimestamp \queryId ->
    FFI.emitGpuZoneBeginSerial locationData queryId contextId
#else
beginSourceLocation _context _location _writeTimestamp = pure $ Zone Nothing
#endif

#ifdef TRACY_ENABLE
{-# INLINE beginWith #-}
beginWith :: MonadIO m => Context -> m Word16 -> (Word16 -> IO ()) -> m Zone
beginWith context writeTimestamp emitBegin = do
  recording <- liftIO isRecording
  if recording then do
    queryId <- writeTimestamp
    liftIO $ emitBegin queryId
    pure . Zone $ Just context
  else
    pure $ Zone Nothing

{-# INLINE isRecording #-}
isRecording :: IO Bool
#ifdef TRACY_ON_DEMAND
isRecording = (/= 0) <$> FFI.connected
#else
isRecording = pure True
#endif
#endif

{-# INLINE end #-}
end :: MonadIO m => Zone -> m Word16 -> m ()
#ifdef TRACY_ENABLE
end (Zone (Just (Context contextId))) writeTimestamp = do
  queryId <- writeTimestamp
  liftIO $ FFI.emitGpuZoneEndSerial queryId contextId
end (Zone Nothing) _writeTimestamp = pure ()
#else
end _zone _writeTimestamp = pure ()
#endif

{-# INLINE time #-}
time :: MonadIO m => Context -> Word16 -> Int64 -> m ()
#ifdef TRACY_ENABLE
time (Context contextId) queryId gpuTime = liftIO $
  FFI.emitGpuTimeSerial gpuTime queryId contextId
#else
time _context _queryId _gpuTime = pure ()
#endif

{-# INLINE calibration #-}
calibration :: MonadIO m => Context -> Int64 -> Int64 -> m ()
#ifdef TRACY_ENABLE
calibration (Context contextId) gpuTime cpuDelta = liftIO $
  FFI.emitGpuCalibrationSerial gpuTime cpuDelta contextId
#else
calibration _context _gpuTime _cpuDelta = pure ()
#endif

{-# INLINE timeSync #-}
timeSync :: MonadIO m => Context -> Int64 -> m ()
#ifdef TRACY_ENABLE
timeSync (Context contextId) gpuTime = liftIO $
  FFI.emitGpuTimeSyncSerial gpuTime contextId
#else
timeSync _context _gpuTime = pure ()
#endif
