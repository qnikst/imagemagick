module Graphics.ImageMagick.MagickWand.PixelWand 
  ( pixelWand
  ) where

import Control.Monad.Trans.Resource
import Graphics.ImageMagick.MagickWand.Types
import Graphics.ImageMagick.MagickWand.FFI.Types
import Graphics.ImageMagick.MagickWand.FFI.PixelWand as F
import Data.ByteString (ByteString)
import Data.ByteString as S
import Foreign

pixelWand :: (MonadResource m) => m PPixelWand
pixelWand = fmap snd (allocate F.newPixelWand destroy)
  where destroy x = F.destroyPixelWand x >> return ()




