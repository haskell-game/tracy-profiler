{-# LANGUAGE CPP #-}

module System.Tracy.FFI.Types
  ( -- * Colors
    Color(..)

    -- * Source locations
  , SrcLoc(..)
  , SourceLocationData(..)
  , pointerBytes

    -- * Contexts
  , TracyCZoneCtx(..)
  , nullTracyCZoneCtx
  , TracyCLockCtx(..)
  , TracyCSharedLockCtx(..)

    -- * Plots
  , PlotFormat(..)

    -- * Messages
  , MessageSeverity(..)

    -- * GPU
  , GpuContextType(..)
  , gpuContextTypeCode

    -- * Text limits
  , maxTextBytes
  , clampTextBytes
  ) where

import Data.Word
import Foreign
import Foreign.C

import WebColor.Labels
import GHC.OverloadedLabels

{- | Packed 32-bit color

Hex literals can be used as @0xRRGGBB@.
Alternatively, "webcolors" notation is supported via labels: @#rgb@, @#rrggbb@, even some @#red@.
-}
newtype Color = Color Word32
  deriving (Show)
  deriving newtype (Eq, Ord, Storable, Num)

instance IsWebColorAlpha s => IsLabel s Color where
  fromLabel = webColorAlpha @s \r g b a ->
    Color $
      shiftL (fromIntegral a) 24 .|.
      shiftL (fromIntegral r) 16 .|.
      shiftL (fromIntegral g)  8 .|.
      fromIntegral b

newtype SrcLoc = SrcLoc Word64
  deriving (Show)
  deriving newtype (Eq, Ord, Storable)

data SourceLocationData = SourceLocationData
  { name     :: Ptr CChar
  , function :: Ptr CChar
  , file     :: Ptr CChar
  , line     :: Word32
  , color    :: Color
  }

pointerBytes :: Int
pointerBytes = sizeOf (nullPtr :: Ptr ())

instance Storable SourceLocationData where
  sizeOf (~_undef) = 3 * pointerBytes + 8

  alignment (~_undef) = pointerBytes

  peek ptr = do
    name     <- peekByteOff ptr 0
    function <- peekByteOff ptr pointerBytes
    file     <- peekByteOff ptr (2 * pointerBytes)
    line     <- peekByteOff ptr (3 * pointerBytes)
    color    <- peekByteOff ptr (3 * pointerBytes + 4)
    pure SourceLocationData{..}

  poke ptr SourceLocationData{..} = do
    pokeByteOff ptr 0 name
    pokeByteOff ptr pointerBytes function
    pokeByteOff ptr (2 * pointerBytes) file
    pokeByteOff ptr (3 * pointerBytes) line
    pokeByteOff ptr (3 * pointerBytes + 4) color

#ifdef TRACY_ON_DEMAND
data TracyCZoneCtx = TracyCZoneCtx
  { idAndActive  :: Word64
  , connectionId :: Word64
  }
  deriving (Eq, Ord, Show)

nullTracyCZoneCtx :: TracyCZoneCtx
nullTracyCZoneCtx = TracyCZoneCtx 0 0
#else
newtype TracyCZoneCtx = TracyCZoneCtx (Ptr TracyCZoneCtx)
  deriving (Show)
  deriving newtype (Eq, Ord, Storable)

nullTracyCZoneCtx :: TracyCZoneCtx
nullTracyCZoneCtx = TracyCZoneCtx nullPtr
#endif

newtype TracyCLockCtx = TracyCLockCtx (Ptr TracyCLockCtx)
  deriving (Show)
  deriving newtype (Eq, Ord, Storable)

newtype TracyCSharedLockCtx = TracyCSharedLockCtx (Ptr TracyCSharedLockCtx)
  deriving (Show)
  deriving newtype (Eq, Ord, Storable)

data PlotFormat
  = PlotFormatNumber -- ^ values will be displayed as plain numbers.
  | PlotFormatMemory -- ^ treats the values as memory sizes. Will display kilobytes, megabytes, etc.
  | PlotFormatPercentage -- ^ values will be displayed as percentage (with value @100@ being equal to 100%).
  | PlotFormatWatt
  deriving (Eq, Ord, Show, Enum, Bounded)

data MessageSeverity
  = MessageSeverityTrace
  | MessageSeverityDebug
  | MessageSeverityInfo
  | MessageSeverityWarning
  | MessageSeverityError
  | MessageSeverityFatal
  deriving (Eq, Ord, Show, Enum, Bounded)

data GpuContextType
  = GpuContextTypeOpenGl
  | GpuContextTypeVulkan
  | GpuContextTypeOpenCL
  | GpuContextTypeDirect3D12
  | GpuContextTypeDirect3D11
  | GpuContextTypeMetal
  | GpuContextTypeCustom
  | GpuContextTypeCUDA
  | GpuContextTypeRocprof
  | GpuContextTypeWebGPU
  deriving (Eq, Ord, Show, Bounded)

gpuContextTypeCode :: GpuContextType -> Word8
gpuContextTypeCode = \case
  GpuContextTypeOpenGl -> 1
  GpuContextTypeVulkan -> 2
  GpuContextTypeOpenCL -> 3
  GpuContextTypeDirect3D12 -> 4
  GpuContextTypeDirect3D11 -> 5
  GpuContextTypeMetal -> 6
  GpuContextTypeCustom -> 7
  GpuContextTypeCUDA -> 8
  GpuContextTypeRocprof -> 9
  GpuContextTypeWebGPU -> 10

maxTextBytes :: Int
maxTextBytes = 65534

clampTextBytes :: Int -> CSize
clampTextBytes = fromIntegral . min maxTextBytes
