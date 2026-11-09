{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

module System.Tracy.FFI where

import Data.Int
import Data.Word
import Foreign.C
import Foreign.C.ConstPtr (ConstPtr(..))

#ifdef TRACY_ON_DEMAND
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff)
#endif

import System.Tracy.FFI.Types

----------------------------------------------------------------

foreign import ccall "tracy/TracyC.h ___tracy_set_thread_name"
  setThreadName
    :: ConstPtr CChar -- ^ name
    -> IO ()

----------------------------------------------------------------

#ifdef TRACY_MANUAL_LIFETIME
foreign import ccall unsafe "___tracy_startup_profiler"
  startupProfiler
    :: IO ()

foreign import ccall unsafe "___tracy_shutdown_profiler"
  shutdownProfiler
    :: IO ()

foreign import ccall unsafe "___tracy_profiler_started"
  profilerStarted
    :: Int
#else
profilerStarted :: Int
profilerStarted = 1
#endif

----------------------------------------------------------------

foreign import ccall unsafe "___tracy_alloc_srcloc"
  allocSrcloc
    :: Word32         -- ^ line
    -> ConstPtr CChar -- ^ source
    -> CSize          -- ^ sourceSz
    -> ConstPtr CChar -- ^ function
    -> CSize          -- ^ functionSz
    -> Color          -- ^ color
    -> IO SrcLoc

foreign import ccall unsafe "___tracy_alloc_srcloc_name"
  allocSrclocName
    :: Word32         -- ^ line
    -> ConstPtr CChar -- ^ source
    -> CSize          -- ^ sourceSz
    -> ConstPtr CChar -- ^ function
    -> CSize          -- ^ functionSz
    -> ConstPtr CChar -- ^ name
    -> CSize          -- ^ nameSz
    -> Color          -- ^ color
    -> IO SrcLoc

----------------------------------------------------------------

#ifdef TRACY_ON_DEMAND
receiveZoneCtx :: (Ptr Word64 -> IO ()) -> IO TracyCZoneCtx
receiveZoneCtx begin =
  allocaArray 2 \out -> do
    begin out
    TracyCZoneCtx <$> peekElemOff out 0 <*> peekElemOff out 1

foreign import ccall unsafe "hs_tracy_emit_zone_begin"
  hsEmitZoneBegin :: ConstPtr SourceLocationData -> Int -> Ptr Word64 -> IO ()

emitZoneBegin :: ConstPtr SourceLocationData -> Int -> IO TracyCZoneCtx
emitZoneBegin srcloc active =
  receiveZoneCtx (hsEmitZoneBegin srcloc active)

foreign import ccall unsafe "hs_tracy_emit_zone_begin_callstack"
  hsEmitZoneBeginCallstack :: ConstPtr SourceLocationData -> Int -> Int -> Ptr Word64 -> IO ()

emitZoneBeginCallstack :: ConstPtr SourceLocationData -> Int -> Int -> IO TracyCZoneCtx
emitZoneBeginCallstack srcloc depth active =
  receiveZoneCtx (hsEmitZoneBeginCallstack srcloc depth active)

foreign import ccall unsafe "hs_tracy_emit_zone_begin_alloc"
  hsEmitZoneBeginAlloc :: SrcLoc -> Int -> Ptr Word64 -> IO ()

emitZoneBeginAlloc :: SrcLoc -> Int -> IO TracyCZoneCtx
emitZoneBeginAlloc srcloc active =
  receiveZoneCtx (hsEmitZoneBeginAlloc srcloc active)

foreign import ccall unsafe "hs_tracy_emit_zone_begin_alloc_callstack"
  hsEmitZoneBeginAllocCallstack :: SrcLoc -> Int -> Int -> Ptr Word64 -> IO ()

emitZoneBeginAllocCallstack :: SrcLoc -> Int -> Int -> IO TracyCZoneCtx
emitZoneBeginAllocCallstack srcloc depth active =
  receiveZoneCtx (hsEmitZoneBeginAllocCallstack srcloc depth active)

foreign import ccall unsafe "hs_tracy_emit_zone_end"
  hsEmitZoneEnd :: Word64 -> Word64 -> IO ()

emitZoneEnd :: TracyCZoneCtx -> IO ()
emitZoneEnd TracyCZoneCtx{..} =
  hsEmitZoneEnd idAndActive connectionId

foreign import ccall unsafe "hs_tracy_emit_zone_text"
  hsEmitZoneText :: Word64 -> Word64 -> ConstPtr CChar -> CSize -> IO ()

emitZoneText :: TracyCZoneCtx -> ConstPtr CChar -> CSize -> IO ()
emitZoneText TracyCZoneCtx{..} =
  hsEmitZoneText idAndActive connectionId

foreign import ccall unsafe "hs_tracy_emit_zone_name"
  hsEmitZoneName :: Word64 -> Word64 -> ConstPtr CChar -> CSize -> IO ()

emitZoneName :: TracyCZoneCtx -> ConstPtr CChar -> CSize -> IO ()
emitZoneName TracyCZoneCtx{..} =
  hsEmitZoneName idAndActive connectionId

foreign import ccall unsafe "hs_tracy_emit_zone_color"
  hsEmitZoneColor :: Word64 -> Word64 -> Color -> IO ()

emitZoneColor :: TracyCZoneCtx -> Color -> IO ()
emitZoneColor TracyCZoneCtx{..} =
  hsEmitZoneColor idAndActive connectionId

foreign import ccall unsafe "hs_tracy_emit_zone_value"
  hsEmitZoneValue :: Word64 -> Word64 -> Word64 -> IO ()

emitZoneValue :: TracyCZoneCtx -> Word64 -> IO ()
emitZoneValue TracyCZoneCtx{..} =
  hsEmitZoneValue idAndActive connectionId
#else
foreign import ccall unsafe "___tracy_emit_zone_begin"
  emitZoneBegin
    :: ConstPtr SourceLocationData -- ^ srcloc
    -> Int                         -- ^ active
    -> IO TracyCZoneCtx

foreign import ccall unsafe "___tracy_emit_zone_begin_callstack"
  emitZoneBeginCallstack
    :: ConstPtr SourceLocationData -- ^ srcloc
    -> Int                         -- ^ depth
    -> Int                         -- ^ active
    -> IO TracyCZoneCtx

foreign import ccall unsafe "___tracy_emit_zone_begin_alloc"
  emitZoneBeginAlloc
    :: SrcLoc -- ^ srcloc
    -> Int    -- ^ active
    -> IO TracyCZoneCtx

foreign import ccall unsafe "___tracy_emit_zone_begin_alloc_callstack"
  emitZoneBeginAllocCallstack
    :: SrcLoc -- ^ srcloc
    -> Int    -- ^ depth
    -> Int    -- ^ active
    -> IO TracyCZoneCtx

foreign import ccall unsafe "___tracy_emit_zone_end"
  emitZoneEnd
    :: TracyCZoneCtx
    -> IO ()

foreign import ccall unsafe "___tracy_emit_zone_text"
  emitZoneText
    :: TracyCZoneCtx
    -> ConstPtr CChar
    -> CSize
    -> IO ()

foreign import ccall unsafe "___tracy_emit_zone_name"
  emitZoneName
    :: TracyCZoneCtx
    -> ConstPtr CChar
    -> CSize
    -> IO ()

foreign import ccall unsafe "___tracy_emit_zone_color"
  emitZoneColor
    :: TracyCZoneCtx
    -> Color
    -> IO ()

foreign import ccall unsafe "___tracy_emit_zone_value"
  emitZoneValue
    :: TracyCZoneCtx
    -> Word64
    -> IO ()
#endif

----------------------------------------------------------------

{-
TRACY_API void ___tracy_emit_gpu_zone_begin( const struct ___tracy_gpu_zone_begin_data );
TRACY_API void ___tracy_emit_gpu_zone_begin_callstack( const struct ___tracy_gpu_zone_begin_callstack_data );
TRACY_API void ___tracy_emit_gpu_zone_begin_alloc( const struct ___tracy_gpu_zone_begin_data );
TRACY_API void ___tracy_emit_gpu_zone_begin_alloc_callstack( const struct ___tracy_gpu_zone_begin_callstack_data );
TRACY_API void ___tracy_emit_gpu_zone_end( const struct ___tracy_gpu_zone_end_data data );
TRACY_API void ___tracy_emit_gpu_time( const struct ___tracy_gpu_time_data );
TRACY_API void ___tracy_emit_gpu_new_context( const struct ___tracy_gpu_new_context_data );
TRACY_API void ___tracy_emit_gpu_context_name( const struct ___tracy_gpu_context_name_data );
TRACY_API void ___tracy_emit_gpu_calibration( const struct ___tracy_gpu_calibration_data );
TRACY_API void ___tracy_emit_gpu_time_sync( const struct ___tracy_gpu_time_sync_data );

TRACY_API void ___tracy_emit_gpu_zone_begin_serial( const struct ___tracy_gpu_zone_begin_data );
TRACY_API void ___tracy_emit_gpu_zone_begin_callstack_serial( const struct ___tracy_gpu_zone_begin_callstack_data );
TRACY_API void ___tracy_emit_gpu_zone_begin_alloc_serial( const struct ___tracy_gpu_zone_begin_data );
TRACY_API void ___tracy_emit_gpu_zone_begin_alloc_callstack_serial( const struct ___tracy_gpu_zone_begin_callstack_data );
TRACY_API void ___tracy_emit_gpu_zone_end_serial( const struct ___tracy_gpu_zone_end_data data );
TRACY_API void ___tracy_emit_gpu_time_serial( const struct ___tracy_gpu_time_data );
TRACY_API void ___tracy_emit_gpu_new_context_serial( const struct ___tracy_gpu_new_context_data );
TRACY_API void ___tracy_emit_gpu_context_name_serial( const struct ___tracy_gpu_context_name_data );
TRACY_API void ___tracy_emit_gpu_calibration_serial( const struct ___tracy_gpu_calibration_data );
TRACY_API void ___tracy_emit_gpu_time_sync_serial( const struct ___tracy_gpu_time_sync_data );
-}

----------------------------------------------------------------

foreign import ccall unsafe "___tracy_connected"
  connected
    :: IO Int

----------------------------------------------------------------

foreign import ccall unsafe "___tracy_emit_memory_alloc"
  emitMemoryAlloc
    :: ConstPtr () -- ^ ptr
    -> CSize       -- ^ size
    -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_alloc_callstack"
  emitMemoryAllocCallstack
    :: ConstPtr () -- ^ ptr
    -> CSize       -- ^ size
    -> CInt        -- ^ depth
    -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_free"
  emitMemoryFree
    :: ConstPtr () -- ^ ptr
    -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_free_callstack"
  emitMemoryFreeCallstack
    :: ConstPtr () -- ^ ptr
    -> CInt        -- ^ depth
    -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_alloc_named"
  emitMemoryAllocNamed
    :: ConstPtr ()    -- ^ ptr
    -> CSize          -- ^ size
    -> ConstPtr CChar -- ^ name
    -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_alloc_callstack_named"
  emitMemoryAllocCallstackNamed
    :: ConstPtr ()    -- ^ ptr
    -> CSize          -- ^ size
    -> CInt           -- ^ depth
    -> ConstPtr CChar -- ^ name
    -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_free_named"
  emitMemoryFreeNamed
    :: ConstPtr ()    -- ^ ptr
    -> ConstPtr CChar -- ^ name
    -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_free_callstack_named"
  emitMemoryFreeCallstackNamed
    :: ConstPtr ()    -- ^ ptr
    -> CInt           -- ^ depth
    -> ConstPtr CChar -- ^ name
    -> IO ()

----------------------------------------------------------------

foreign import ccall unsafe "___tracy_emit_logString"
  emitLogString
    :: Int8
    -> Color
    -> CInt
    -> CSize
    -> ConstPtr CChar
    -> IO ()

foreign import ccall unsafe "___tracy_emit_logStringL"
  emitLogStringL
    :: Int8
    -> Color
    -> CInt
    -> ConstPtr CChar
    -> IO ()

----------------------------------------------------------------

foreign import ccall unsafe "___tracy_emit_frame_mark"
  emitFrameMark
    :: ConstPtr CChar -- ^ name
    -> IO ()

foreign import ccall unsafe "___tracy_emit_frame_mark_start"
  emitFrameMarkStart
    :: ConstPtr CChar -- ^ name
    -> IO ()

foreign import ccall unsafe "___tracy_emit_frame_mark_end"
  emitFrameMarkEnd
    :: ConstPtr CChar -- ^ name
    -> IO ()

foreign import ccall unsafe "___tracy_emit_frame_image"
  emitFrameImage
    :: ConstPtr () -- ^ image
    -> Word16      -- ^ w
    -> Word16      -- ^ h
    -> Word8       -- ^ offset
    -> CInt        -- ^ flip
    -> IO ()

----------------------------------------------------------------

foreign import ccall unsafe "___tracy_emit_plot"
  emitPlot
    :: ConstPtr CChar -- ^ name
    -> Double         -- ^ val
    -> IO ()

foreign import ccall unsafe "___tracy_emit_plot_float"
  emitPlotFloat
    :: ConstPtr CChar -- ^ name
    -> Float          -- ^ val
    -> IO ()

foreign import ccall unsafe "___tracy_emit_plot_int"
  emitPlotInt
    :: ConstPtr CChar -- ^ name
    -> Int64          -- ^ val
    -> IO ()

foreign import ccall unsafe "___tracy_emit_plot_config"
  emitPlotConfig
    :: ConstPtr CChar -- ^ name
    -> CInt           -- ^ type
    -> CInt           -- ^ step
    -> CInt           -- ^ fill
    -> Color          -- ^ color
    -> IO ()

-- XXX: yes, in the same block with emit_plot
foreign import ccall unsafe "___tracy_emit_message_appinfo"
  emitMessageAppinfo
    :: ConstPtr CChar -- ^ txt
    -> CSize          -- ^ size
    -> IO ()

----------------------------------------------------------------

{-
TRACY_API struct __tracy_lockable_context_data* ___tracy_announce_lockable_ctx( const struct ___tracy_source_location_data* srcloc );
TRACY_API void ___tracy_terminate_lockable_ctx( struct __tracy_lockable_context_data* lockdata );
TRACY_API int ___tracy_before_lock_lockable_ctx( struct __tracy_lockable_context_data* lockdata );
TRACY_API void ___tracy_after_lock_lockable_ctx( struct __tracy_lockable_context_data* lockdata );
TRACY_API void ___tracy_before_unlock_lockable_ctx( struct __tracy_lockable_context_data* lockdata );
TRACY_API void ___tracy_after_try_lock_lockable_ctx( struct __tracy_lockable_context_data* lockdata, int acquired );
TRACY_API void ___tracy_mark_lockable_ctx( struct __tracy_lockable_context_data* lockdata, const struct ___tracy_source_location_data* srcloc );
TRACY_API void ___tracy_custom_name_lockable_ctx( struct __tracy_lockable_context_data* lockdata, const char* name, size_t nameSz );
-}

----------------------------------------------------------------

#ifdef TRACY_FIBERS
foreign import ccall unsafe "___tracy_fiber_enter"
  fiberEnter
    :: ConstPtr CChar -- ^ fiber
    -> IO ()

foreign import ccall unsafe "___tracy_fiber_leave"
  fiberLeave
    :: IO ()
#endif
