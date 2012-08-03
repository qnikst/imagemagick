module Graphics.ImageMagick.MagickWand.PixelWand
  ( pixelWand
  , setColor
  ) where

import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                               (ByteString)
import           Data.ByteString                               as S
-- import           Foreign
import           Graphics.ImageMagick.MagickWand.FFI.PixelWand as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import           Graphics.ImageMagick.MagickWand.Types

pixelWand :: (MonadResource m) => m PPixelWand
pixelWand = fmap snd (allocate F.newPixelWand destroy)
  where destroy = void . F.destroyPixelWand

setColor :: (MonadResource m) => PPixelWand -> ByteString -> m Bool
setColor p s = liftIO $ useAsCString s (F.pixelSetColor p) >>= return . (==mTrue)


