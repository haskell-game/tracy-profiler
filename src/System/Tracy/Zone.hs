{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}

module System.Tracy.Zone
  ( -- * Declare zones
    withSrcLoc
  , withSourceLocation

    -- * Update zone context
  , text
  , name
  , color
  , value

    -- * Internals
  , withSrcLocImpl
  , allocSrcloc
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word

#ifdef TRACY_ENABLE
import Control.Exception (bracket)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Text.Foreign qualified as Text
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.C.String (peekCAString)
import Foreign.Storable (peek)
import GHC.Stack
import GHC.Stack.Types qualified as GHC

#ifndef ZONES_UNSAFE
import Control.Concurrent (isCurrentThreadBound, rtsSupportsBoundThreads)
-- !unsafe
#endif
#ifdef ZONES_PEDANTIC
import System.Exit (die)
-- pedantic
#endif

import System.Tracy.FFI qualified as FFI
-- enable
#endif

import System.Tracy.FFI.Types qualified as FFI
import System.Tracy.SourceLocation (SourceLocation(..))

{- | Allocate SrcLoc and run a Zone with it.

It will produce a @?zoneCtx@ implicit for the zone functions to work.

@
{-# LANGUAGE OverloadedLabels #-} -- for colors
{-# LANGUAGE OverloadedStrings #-} -- for names

import System.Tracy.Zone qualified as Zone

rendering = Zone.withSrcLoc "rendering" #yellow do
  -- ...
@

NB: With the threaded runtime, zones can only be used on a bound thread.
Main thread is safe, but the rest should use one of forkOS/asyncBound/runInBoundThread.

The non-threaded runtime has no bound threads and runs everything on a single OS thread, so the check is skipped there.
Zones from different Haskell threads must not overlap in that case, or the capture will fail.
-}
{-# INLINE withSrcLoc #-}
withSrcLoc
#ifdef TRACY_ENABLE
  :: ( HasCallStack
     , MonadUnliftIO m
     )
#else
  :: ()
#endif
  => ByteString -- ^ Function name (if used as a top-level wrapper) or section name (in a middle of a @do@ block).
  -> FFI.Color
  -> ((?zoneCtx :: FFI.TracyCZoneCtx) => m a)
  -> m a
#ifndef TRACY_ENABLE
withSrcLoc _function _col action = let ?zoneCtx = FFI.nullTracyCZoneCtx in action
#else
withSrcLoc function col action = withRunInIO \inIO -> do
  -- XXX: breaking into non-public API from GHC.Stack.Types to short-circuit `srcLocFile` stringification
  case callStack of
    GHC.PushCallStack _myself GHC.SrcLoc{srcLocStartLine, srcLocFile} _next ->
      withSrcLocImpl
        (fromIntegral srcLocStartLine)
        (ByteString.pack srcLocFile) -- XXX: srcLocFile is originally an Addr#. So pack should be optimized down to FinalPtr too.
        function -- XXX: IsString instance has an unsafePackLiteral rule for string literals
        col
        inIO
        action
    _EmptyOrFreeze ->
      error "withSrcLoc ought to have HasCallStack in context"
#endif

{- | Allocate SrcLoc and run a Zone with it.

This will copy the strings into a one-time temporary buffer and feed it into emitZoneBeginAlloc.
You'd better have those bytestrings come from static literals to avoid even more intermediate allocations.
-}
{-# INLINE withSrcLocImpl #-}
withSrcLocImpl
  :: Word32
  -> ByteString
  -> ByteString
  -> FFI.Color
  -> (m a -> IO a)
  -> ((?zoneCtx :: FFI.TracyCZoneCtx) => m a)
  -> IO a
#ifndef TRACY_ENABLE
withSrcLocImpl _line _file _function _col inIO action =
  inIO $ let ?zoneCtx = FFI.nullTracyCZoneCtx in action
#else
withSrcLocImpl line file function col inIO action =
  runZone
    (pure $ ByteString.unpack file <> ":" <> show line)
    inIO
    (allocSrcloc line file function Nothing col >>= \srcloc -> FFI.emitZoneBeginAlloc srcloc 1)
    action
-- TRACY_ENABLE
#endif

{-# INLINE withSourceLocation #-}
#ifdef TRACY_ENABLE
withSourceLocation
  :: MonadUnliftIO m
  => SourceLocation
  -> ((?zoneCtx :: FFI.TracyCZoneCtx) => m a)
  -> m a
withSourceLocation location@(SourceLocation locationData) action = withRunInIO \inIO ->
  runZone
    (describeSourceLocation location)
    inIO
    (FFI.emitZoneBegin locationData 1)
    action
#else
withSourceLocation
  :: SourceLocation
  -> ((?zoneCtx :: FFI.TracyCZoneCtx) => m a)
  -> m a
withSourceLocation _location action = let ?zoneCtx = FFI.nullTracyCZoneCtx in action
#endif

#ifdef TRACY_ENABLE
{-# INLINE runZone #-}
runZone
  :: IO String
  -> (m a -> IO a)
  -> IO FFI.TracyCZoneCtx
  -> ((?zoneCtx :: FFI.TracyCZoneCtx) => m a)
  -> IO a
runZone location inIO beginZone action = do
  onStableOsThread <- canStartZone
  if onStableOsThread then
    bracket beginZone FFI.emitZoneEnd \ctx -> inIO $ let ?zoneCtx = ctx in action
  else
    unboundZone location . inIO $ let ?zoneCtx = FFI.nullTracyCZoneCtx in action

{-# INLINE canStartZone #-}
canStartZone :: IO Bool
#ifdef ZONES_UNSAFE
canStartZone = pure True
#else
canStartZone = if rtsSupportsBoundThreads then isCurrentThreadBound else pure True
#endif

unboundZone :: IO String -> IO a -> IO a
#ifdef ZONES_PEDANTIC
unboundZone location _withoutZone = do
  {-
    XXX: Will not actually crash the whole program, only the thread.
    But at least there would be a console notice when this happens.
  -}
  zoneLocation <- location
  die $ zoneLocation <> " Starting a zone on unbound thread"
#else
unboundZone _location withoutZone = withoutZone
#endif

describeSourceLocation :: SourceLocation -> IO String
describeSourceLocation (SourceLocation (ConstPtr locationData)) = do
  FFI.SourceLocationData{file, line} <- peek locationData
  fileName <- peekCAString file
  pure $ fileName <> ":" <> show line
#endif

{- | Prepare a single-use location identifier

Returns a source location identifier corresponding to an *allocated source location*.
As these functions do not require the provided string data to be available after they return, the calling code is free to deallocate them at any time afterward.
This way, the string lifetime requirements described in section 3.1 are relaxed.

The variable representing an allocated source location is of an opaque type.
After it is passed to one of the zone begin functions, its value *cannot be reused* (the variable is consumed).
You must allocate a new source location for each zone begin event, even if the location data would be the same as in the previous instance.
-}
allocSrcloc
  :: Word32
  -> ByteString
  -> ByteString
  -> Maybe ByteString
  -> FFI.Color
  -> IO FFI.SrcLoc
#ifndef TRACY_ENABLE
allocSrcloc _line _source _function _name _col = pure $ FFI.SrcLoc 0
#else
allocSrcloc line source function name_ col =
  unsafeUseAsCStringLen source \(sourcePtr, sourceSz) ->
  unsafeUseAsCStringLen function \(functionPtr, functionSz) ->
    case name_ of
      Nothing ->
        FFI.allocSrcloc
          line
          (ConstPtr sourcePtr) (fromIntegral sourceSz)
          (ConstPtr functionPtr) (fromIntegral functionSz)
          col
      Just name' ->
        unsafeUseAsCStringLen name' \(namePtr, nameSz) ->
          FFI.allocSrclocName
            line
            (ConstPtr sourcePtr) (fromIntegral sourceSz)
            (ConstPtr functionPtr) (fromIntegral functionSz)
            (ConstPtr namePtr) (fromIntegral nameSz)
            col
#endif

{-# INLINE text #-}
#ifndef TRACY_ENABLE
text :: MonadIO m => Text -> m ()
text _txt = pure ()
#else
text :: (MonadIO m, ?zoneCtx :: FFI.TracyCZoneCtx) => Text -> m ()
text txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.emitZoneText ?zoneCtx (ConstPtr txtPtr) (FFI.clampTextBytes txtSz)
#endif

{-# INLINE name #-}
#ifndef TRACY_ENABLE
name :: MonadIO m => Text -> m ()
name _txt = pure ()
#else
name :: (MonadIO m, ?zoneCtx :: FFI.TracyCZoneCtx) => Text -> m ()
name txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.emitZoneName ?zoneCtx (ConstPtr txtPtr) (FFI.clampTextBytes txtSz)
#endif

{-# INLINE color #-}
#ifndef TRACY_ENABLE
color :: MonadIO m => FFI.Color -> m ()
color _col = pure ()
#else
color :: (MonadIO m, ?zoneCtx :: FFI.TracyCZoneCtx) => FFI.Color -> m ()
color col = liftIO $ FFI.emitZoneColor ?zoneCtx col
#endif

{-# INLINE value #-}
#ifndef TRACY_ENABLE
value :: MonadIO m => Word64 -> m ()
value _val = pure ()
#else
value :: (MonadIO m, ?zoneCtx :: FFI.TracyCZoneCtx) => Word64 -> m ()
value val = liftIO $ FFI.emitZoneValue ?zoneCtx val
#endif
