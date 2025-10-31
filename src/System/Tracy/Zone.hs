{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}

module System.Tracy.Zone
  ( -- * Declare zones
    withSrcLoc_

    -- * Update zone context
  , text
  , name
  , color
  , value

    -- * Internals
  , allocSrcloc
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Text (Text)
import Data.Text.Foreign qualified as Text
import Data.Word
import Foreign.C.ConstPtr (ConstPtr(..))

#ifdef TRACY_ENABLE
import Control.Exception (bracket)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

#ifndef ZONES_UNSAFE
import Control.Concurrent (isCurrentThreadBound)
#ifdef ZONES_PEDANTIC
import Data.ByteString.Char8 qualified as ByteString
import System.Exit (die)
#endif
#endif
#endif

import System.Tracy.FFI qualified as FFI
import System.Tracy.FFI.Types qualified as FFI

{- | Allocate SrcLoc and run a Zone with it.

It will produce a @?zoneCtx@ implicit for the zone functions to work.

@
{-# LANGUAGE CPP #-}

import System.Tracy.Zone qualified as Zone

rendering = Zone.withSrcLoc_ \_\_LINE\_\_ \_\_FILE\_\_ "rendering" #yellow do
  -- ...
@
-}
{-# INLINE withSrcLoc_ #-}
withSrcLoc_ ::
#ifndef TRACY_ENABLE
  ()
#else
  (MonadUnliftIO m)
#endif
  => Word32
  -> ByteString
  -> ByteString
  -> FFI.Color
  -> ((?zoneCtx :: FFI.TracyCZoneCtx) => m a)
  -> m a
#ifndef TRACY_ENABLE
withSrcLoc_ _line _file _function _col action =
  let ?zoneCtx = FFI.nullTracyCZoneCtx
  in action
#else
withSrcLoc_ line file function col action = withRunInIO \inIO -> do

#ifdef ZONES_UNSAFE
  bound <- isCurrentThreadBound
  putStrLn $ "ZONES_UNSAFE: " <> show bound
  runZone inIO
#else
  bound <- isCurrentThreadBound
  if bound then
    runZone inIO
  else
#ifdef ZONES_PEDANTIC
    {-
      XXX: Will not actually crash the whole program, only the thread.
      But at least there would be a console notice when this happens.
    -}
    die $ ByteString.unpack file <> ":" <> show line <> " Starting a zone on unbound thread"
#else
    inIO $ let ?zoneCtx = FFI.nullTracyCZoneCtx in action
#endif

#endif
  where
    {-# INLINE runZone #-}
    runZone inIO = do
      srcloc <- allocSrcloc line file function Nothing col
      bracket
        (FFI.emitZoneBeginAlloc srcloc 1)
        FFI.emitZoneEnd
        (\ctx -> inIO $ let ?zoneCtx = ctx in action)
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

{- TODO: Wrap emitZoneBegin

This needs keeping SourceLocationData structures filled with pinned pointers.
Otherwise the data would be pulled much later, outside a potential with/bracket scope
with garbage/crashes as a result.

Some nice solution would require interning SourceLocationData and its data.
Otherwise it's a copying galore and Tracy may fail to deduplicate the locations.
-}

{-# INLINE text #-}
text :: (MonadIO m, ?zoneCtx :: FFI.TracyCZoneCtx) => Text -> m ()
text txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.emitZoneText ?zoneCtx (ConstPtr txtPtr) (fromIntegral txtSz)

{-# INLINE name #-}
name :: (MonadIO m, ?zoneCtx :: FFI.TracyCZoneCtx) => Text -> m ()
name txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.emitZoneName ?zoneCtx (ConstPtr txtPtr) (fromIntegral txtSz)

{-# INLINE color #-}
color :: (MonadIO m, ?zoneCtx :: FFI.TracyCZoneCtx) => FFI.Color -> m ()
color col = liftIO $ FFI.emitZoneColor ?zoneCtx col

{-# INLINE value #-}
value :: (MonadIO m, ?zoneCtx :: FFI.TracyCZoneCtx) => Word64 -> m ()
value val = liftIO $ FFI.emitZoneValue ?zoneCtx val
