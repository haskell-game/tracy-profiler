{-# LANGUAGE CPP #-}

module System.Tracy.Section
  ( -- * Scoped sections
    with
  , withCategory

    -- * Manual sections
  , Section(..)
  , enter
  , enterCategory
  , leave

    -- * Categories
  , setup
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import Data.Word (Word16, Word32)

#ifdef TRACY_ENABLE
import Control.Exception (bracket)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Text.Foreign qualified as Text
import Foreign.C.ConstPtr (ConstPtr(..))

import System.Tracy.FFI qualified as FFI
import System.Tracy.FFI.Types (clampTextBytes)
#endif

newtype Section = Section Word32

{-# INLINE enter #-}
enter :: MonadIO m => Text -> m Section
enter = enterCategory 0

{-# INLINE enterCategory #-}
enterCategory :: MonadIO m => Word16 -> Text -> m Section
#ifdef TRACY_ENABLE
enterCategory category txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    Section <$> FFI.sectionEnter category (ConstPtr txtPtr) (clampTextBytes txtSz)
#else
enterCategory _category _txt = pure $ Section 0
#endif

{-# INLINE leave #-}
leave :: MonadIO m => Section -> m ()
#ifdef TRACY_ENABLE
leave (Section sectionId) = liftIO $ FFI.sectionLeave sectionId
#else
leave _section = pure ()
#endif

{-# INLINE setup #-}
setup :: MonadIO m => Word16 -> Text -> m ()
#ifdef TRACY_ENABLE
setup category txt = liftIO $
  Text.withCStringLen txt \(txtPtr, txtSz) ->
    FFI.sectionSetup category (ConstPtr txtPtr) (clampTextBytes txtSz)
#else
setup _category _txt = pure ()
#endif

#ifdef TRACY_ENABLE
with :: MonadUnliftIO m => Text -> m a -> m a
with = withCategory 0

withCategory :: MonadUnliftIO m => Word16 -> Text -> m a -> m a
withCategory category txt action =
  withRunInIO \run ->
    bracket (enterCategory category txt) leave \_section -> run action
#else
with :: Text -> a -> a
with _txt action = action

withCategory :: Word16 -> Text -> a -> a
withCategory _category _txt action = action
#endif
