module Graphics.ImageMagick.MagickWand.PixelWand 
  ( pixelWand
  , setColor
  ) where

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Graphics.ImageMagick.MagickWand.Types
import Graphics.ImageMagick.MagickWand.FFI.Types
import Graphics.ImageMagick.MagickWand.FFI.PixelWand as F
import Data.ByteString (ByteString)
import Data.ByteString as S
import Foreign

pixelWand :: (MonadResource m) => m PPixelWand
pixelWand = fmap snd (allocate F.newPixelWand destroy)
  where destroy x = F.destroyPixelWand x >> return ()

setColor :: (MonadResource m) => PPixelWand -> ByteString -> m (Bool)
setColor p s = liftIO $ useAsCString s (F.pixelSetColor p) >>= return . (==mTrue)


