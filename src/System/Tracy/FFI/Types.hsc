module System.Tracy.FFI.Types where

import Data.Word
import Foreign
import Foreign.C

import WebColor.Labels
import GHC.OverloadedLabels

#define TRACY_ENABLE
#include <tracy/TracyC.h>

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
  , color    :: Word32
  }

instance Storable SourceLocationData where
  sizeOf (~_undef) = (#size struct ___tracy_source_location_data)

  alignment (~_undef) = (#alignment struct ___tracy_source_location_data)

  peek ptr = do
    name     <- (#peek struct ___tracy_source_location_data, name) ptr
    function <- (#peek struct ___tracy_source_location_data, function) ptr
    file     <- (#peek struct ___tracy_source_location_data, file) ptr
    line     <- (#peek struct ___tracy_source_location_data, line) ptr
    color    <- (#peek struct ___tracy_source_location_data, color) ptr
    pure SourceLocationData{..}

  poke ptr SourceLocationData{..} = do
    (#poke struct ___tracy_source_location_data, name) ptr name
    (#poke struct ___tracy_source_location_data, function) ptr function
    (#poke struct ___tracy_source_location_data, file) ptr file
    (#poke struct ___tracy_source_location_data, line) ptr line
    (#poke struct ___tracy_source_location_data, color) ptr color

newtype TracyCZoneCtx = TracyCZoneCtx (Ptr TracyCZoneCtx)
  deriving (Show)
  deriving newtype (Eq, Ord, Storable)

nullTracyCZoneCtx :: TracyCZoneCtx
nullTracyCZoneCtx = TracyCZoneCtx nullPtr

data PlotFormat
  = PlotFormatNumber -- ^ values will be displayed as plain numbers.
  | PlotFormatMemory -- ^ treats the values as memory sizes. Will display kilobytes, megabytes, etc.
  | PlotFormatPercentage -- ^ values will be displayed as percentage (with value @100@ being equal to 100%).
  | PlotFormatWatt
  deriving (Eq, Ord, Show, Enum, Bounded)
