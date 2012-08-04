module Graphics.ImageMagick.MagickWand.PixelWand
  ( pixelWand
  , setColor
  ) where

import           Control.Monad                                 (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                               (ByteString, useAsCString)

import           Graphics.ImageMagick.MagickWand.FFI.PixelWand as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils

pixelWand :: (MonadResource m) => m PPixelWand
pixelWand = fmap snd (allocate F.newPixelWand destroy)
  where destroy = void . F.destroyPixelWand

setColor :: (MonadResource m) => PPixelWand -> ByteString -> m ()
setColor p s = withException_ p $ useAsCString s (F.pixelSetColor p)


