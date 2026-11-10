{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

module System.Tracy.FFI
  ( -- * Profiler
    profilerStarted
#ifdef TRACY_MANUAL_LIFETIME
  , startupProfiler
  , shutdownProfiler
#endif
  , connected

    -- * Threads
  , setThreadName

    -- * Source locations
  , allocSrcloc
  , allocSrclocName

    -- * Zones
  , emitZoneBegin
  , emitZoneBeginCallstack
  , emitZoneBeginAlloc
  , emitZoneBeginAllocCallstack
  , emitZoneEnd
  , emitZoneText
  , emitZoneName
  , emitZoneColor
  , emitZoneValue
#ifdef TRACY_ON_DEMAND

    -- * On-demand zone shim
  , receiveZoneCtx
  , hsEmitZoneBegin
  , hsEmitZoneBeginCallstack
  , hsEmitZoneBeginAlloc
  , hsEmitZoneBeginAllocCallstack
  , hsEmitZoneEnd
  , hsEmitZoneText
  , hsEmitZoneName
  , hsEmitZoneColor
  , hsEmitZoneValue
#endif

    -- * GPU
  , emitGpuZoneBegin
  , emitGpuZoneBeginCallstack
  , emitGpuZoneBeginAlloc
  , emitGpuZoneBeginAllocCallstack
  , emitGpuZoneEnd
  , emitGpuTime
  , emitGpuNewContext
  , emitGpuContextName
  , emitGpuCalibration
  , emitGpuTimeSync

    -- * Serialized GPU
  , emitGpuZoneBeginSerial
  , emitGpuZoneBeginCallstackSerial
  , emitGpuZoneBeginAllocSerial
  , emitGpuZoneBeginAllocCallstackSerial
  , emitGpuZoneEndSerial
  , emitGpuTimeSerial
  , emitGpuNewContextSerial
  , emitGpuContextNameSerial
  , emitGpuCalibrationSerial
  , emitGpuTimeSyncSerial

    -- * Memory
  , emitMemoryAlloc
  , emitMemoryAllocCallstack
  , emitMemoryFree
  , emitMemoryFreeCallstack
  , emitMemoryAllocNamed
  , emitMemoryAllocCallstackNamed
  , emitMemoryFreeNamed
  , emitMemoryFreeCallstackNamed
  , emitMemoryDiscard
  , emitMemoryDiscardCallstack

    -- * Messages
  , emitLogString
  , emitLogStringL
  , emitMessageAppinfo

    -- * Frames
  , emitFrameMark
  , emitFrameMarkStart
  , emitFrameMarkEnd
  , emitFrameImage

    -- * Plots
  , emitPlot
  , emitPlotFloat
  , emitPlotInt
  , emitPlotConfig

    -- * Locks
  , announceLockableCtx
  , terminateLockableCtx
  , beforeLockLockableCtx
  , afterLockLockableCtx
  , beforeUnlockLockableCtx
  , afterTryLockLockableCtx
  , markLockableCtx
  , customNameLockableCtx

    -- * Shared locks
  , announceSharedLockableCtx
  , terminateSharedLockableCtx
  , beforeLockSharedLockableCtx
  , afterLockSharedLockableCtx
  , beforeUnlockSharedLockableCtx
  , afterTryLockSharedLockableCtx
  , beforeLockSharedSharedLockableCtx
  , afterLockSharedSharedLockableCtx
  , beforeUnlockSharedSharedLockableCtx
  , afterTryLockSharedSharedLockableCtx
  , markSharedLockableCtx
  , customNameSharedLockableCtx

    -- * Sections
  , sectionEnter
  , sectionLeave
  , sectionSetup

    -- * Time
  , getTime

    -- * Sampling profiler
  , beginSamplingProfiling
  , endSamplingProfiling

#ifdef TRACY_FIBERS
    -- * Fibers
  , fiberEnter
  , fiberLeave
#endif
  ) where

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
    :: IO Int32
#else
profilerStarted :: IO Int32
profilerStarted = pure 1
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

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_begin"
  emitGpuZoneBegin :: ConstPtr SourceLocationData -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_begin_callstack"
  emitGpuZoneBeginCallstack :: ConstPtr SourceLocationData -> Int32 -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_begin_alloc"
  emitGpuZoneBeginAlloc :: SrcLoc -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_begin_alloc_callstack"
  emitGpuZoneBeginAllocCallstack :: SrcLoc -> Int32 -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_end"
  emitGpuZoneEnd :: Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_time"
  emitGpuTime :: Int64 -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_new_context"
  emitGpuNewContext :: Int64 -> Float -> Word8 -> Word8 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_context_name"
  emitGpuContextName :: Word8 -> ConstPtr CChar -> Word16 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_calibration"
  emitGpuCalibration :: Int64 -> Int64 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_time_sync"
  emitGpuTimeSync :: Int64 -> Word8 -> IO ()

----------------------------------------------------------------

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_begin_serial"
  emitGpuZoneBeginSerial :: ConstPtr SourceLocationData -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_begin_callstack_serial"
  emitGpuZoneBeginCallstackSerial :: ConstPtr SourceLocationData -> Int32 -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_begin_alloc_serial"
  emitGpuZoneBeginAllocSerial :: SrcLoc -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_begin_alloc_callstack_serial"
  emitGpuZoneBeginAllocCallstackSerial :: SrcLoc -> Int32 -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_zone_end_serial"
  emitGpuZoneEndSerial :: Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_time_serial"
  emitGpuTimeSerial :: Int64 -> Word16 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_new_context_serial"
  emitGpuNewContextSerial :: Int64 -> Float -> Word8 -> Word8 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_context_name_serial"
  emitGpuContextNameSerial :: Word8 -> ConstPtr CChar -> Word16 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_calibration_serial"
  emitGpuCalibrationSerial :: Int64 -> Int64 -> Word8 -> IO ()

foreign import ccall unsafe "hs_tracy_emit_gpu_time_sync_serial"
  emitGpuTimeSyncSerial :: Int64 -> Word8 -> IO ()

----------------------------------------------------------------

foreign import ccall unsafe "___tracy_connected"
  connected
    :: IO Int32

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

foreign import ccall unsafe "___tracy_emit_memory_discard"
  emitMemoryDiscard :: ConstPtr CChar -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_discard_callstack"
  emitMemoryDiscardCallstack :: ConstPtr CChar -> CInt -> IO ()

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

foreign import ccall unsafe "___tracy_announce_lockable_ctx"
  announceLockableCtx :: ConstPtr SourceLocationData -> IO TracyCLockCtx

foreign import ccall unsafe "___tracy_terminate_lockable_ctx"
  terminateLockableCtx :: TracyCLockCtx -> IO ()

foreign import ccall unsafe "___tracy_before_lock_lockable_ctx"
  beforeLockLockableCtx :: TracyCLockCtx -> IO Int32

foreign import ccall unsafe "___tracy_after_lock_lockable_ctx"
  afterLockLockableCtx :: TracyCLockCtx -> IO ()

foreign import ccall unsafe "___tracy_before_unlock_lockable_ctx"
  beforeUnlockLockableCtx :: TracyCLockCtx -> IO ()

foreign import ccall unsafe "___tracy_after_try_lock_lockable_ctx"
  afterTryLockLockableCtx :: TracyCLockCtx -> Int32 -> IO ()

foreign import ccall unsafe "___tracy_mark_lockable_ctx"
  markLockableCtx :: TracyCLockCtx -> ConstPtr SourceLocationData -> IO ()

foreign import ccall unsafe "___tracy_custom_name_lockable_ctx"
  customNameLockableCtx :: TracyCLockCtx -> ConstPtr CChar -> CSize -> IO ()

----------------------------------------------------------------

foreign import ccall unsafe "___tracy_announce_shared_lockable_ctx"
  announceSharedLockableCtx :: ConstPtr SourceLocationData -> IO TracyCSharedLockCtx

foreign import ccall unsafe "___tracy_terminate_shared_lockable_ctx"
  terminateSharedLockableCtx :: TracyCSharedLockCtx -> IO ()

foreign import ccall unsafe "___tracy_before_lock_shared_lockable_ctx"
  beforeLockSharedLockableCtx :: TracyCSharedLockCtx -> IO Int32

foreign import ccall unsafe "___tracy_after_lock_shared_lockable_ctx"
  afterLockSharedLockableCtx :: TracyCSharedLockCtx -> IO ()

foreign import ccall unsafe "___tracy_before_unlock_shared_lockable_ctx"
  beforeUnlockSharedLockableCtx :: TracyCSharedLockCtx -> IO ()

foreign import ccall unsafe "___tracy_after_try_lock_shared_lockable_ctx"
  afterTryLockSharedLockableCtx :: TracyCSharedLockCtx -> Int32 -> IO ()

foreign import ccall unsafe "___tracy_before_lock_shared_shared_lockable_ctx"
  beforeLockSharedSharedLockableCtx :: TracyCSharedLockCtx -> IO Int32

foreign import ccall unsafe "___tracy_after_lock_shared_shared_lockable_ctx"
  afterLockSharedSharedLockableCtx :: TracyCSharedLockCtx -> IO ()

foreign import ccall unsafe "___tracy_before_unlock_shared_shared_lockable_ctx"
  beforeUnlockSharedSharedLockableCtx :: TracyCSharedLockCtx -> IO ()

foreign import ccall unsafe "___tracy_after_try_lock_shared_shared_lockable_ctx"
  afterTryLockSharedSharedLockableCtx :: TracyCSharedLockCtx -> Int32 -> IO ()

foreign import ccall unsafe "___tracy_mark_shared_lockable_ctx"
  markSharedLockableCtx :: TracyCSharedLockCtx -> ConstPtr SourceLocationData -> IO ()

foreign import ccall unsafe "___tracy_custom_name_shared_lockable_ctx"
  customNameSharedLockableCtx :: TracyCSharedLockCtx -> ConstPtr CChar -> CSize -> IO ()

----------------------------------------------------------------

foreign import ccall unsafe "hs_tracy_section_enter"
  sectionEnter :: Word16 -> ConstPtr CChar -> CSize -> IO Word32

foreign import ccall unsafe "___tracy_section_leave"
  sectionLeave :: Word32 -> IO ()

foreign import ccall unsafe "hs_tracy_section_setup"
  sectionSetup :: Word16 -> ConstPtr CChar -> CSize -> IO ()

----------------------------------------------------------------

foreign import ccall unsafe "___tracy_get_time"
  getTime :: IO Int64

foreign import ccall safe "___tracy_begin_sampling_profiling"
  beginSamplingProfiling :: IO CInt

foreign import ccall safe "___tracy_end_sampling_profiling"
  endSamplingProfiling :: IO ()

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
