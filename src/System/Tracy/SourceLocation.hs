{-# LANGUAGE CPP #-}

module System.Tracy.SourceLocation
  ( SourceLocation(..)
  , newSourceLocation
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.Ptr (nullPtr)

#ifdef TRACY_ENABLE
import Data.ByteString.Char8 qualified as ByteString
import Foreign.C.String (newCAString)
import Foreign.Marshal.Utils (new)
import GHC.Stack (HasCallStack, SrcLoc(..), callStack, getCallStack)
#endif

import System.Tracy.FFI.Types qualified as FFI

newtype SourceLocation = SourceLocation (ConstPtr FFI.SourceLocationData)

#ifdef TRACY_ENABLE
newSourceLocation :: (HasCallStack, MonadIO m) => ByteString -> FFI.Color -> m SourceLocation
newSourceLocation function color = liftIO case getCallStack callStack of
  (_called, SrcLoc{srcLocFile, srcLocStartLine}) : _callers -> do
    functionName <- newCAString (ByteString.unpack function)
    fileName <- newCAString srcLocFile
    SourceLocation . ConstPtr <$>
      new (FFI.SourceLocationData nullPtr functionName fileName (fromIntegral srcLocStartLine) color)
  [] ->
    error "newSourceLocation ought to have HasCallStack in context"
#else
newSourceLocation :: MonadIO m => ByteString -> FFI.Color -> m SourceLocation
newSourceLocation _function _color = pure $ SourceLocation (ConstPtr nullPtr)
#endif
