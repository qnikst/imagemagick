module Graphics.ImageMagick.MagickCore.Mime
  ( toMime
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                          (packCString,
                                                           useAsCString)
import           Data.Text                                (Text)
import           Data.Text.Encoding                       (decodeUtf8,
                                                           encodeUtf8)
import           Foreign
import           Prelude


import qualified Graphics.ImageMagick.MagickCore.FFI.Mime as F

toMime :: (MonadResource m) => Text -> m Text
toMime format = liftIO $ do
  cstr <- useAsCString (encodeUtf8 format) F.magickToMime
  mime <- decodeUtf8 <$> packCString cstr
  free cstr
  return mime
