module Graphics.ImageMagick.MagickWand.WandImage
  ( getImageHeight
  , getImageWidth
  , resizeImage
  , getImageCompressionQuality
  , setImageCompressionQuality
  , getImageBackgroundColor
  , setImageBackgroundColor
  , extentImage
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Foreign
import Foreign.C.Types
import Graphics.ImageMagick.MagickWand.PixelWand
import Graphics.ImageMagick.MagickWand.Types
import Graphics.ImageMagick.MagickWand.FFI.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.MagickWand  as F
import qualified Graphics.ImageMagick.MagickWand.FFI.WandImage   as F

getImageHeight :: (MonadResource m) => Ptr MagickWand -> m Int
getImageHeight w = liftIO $ fmap fromIntegral (F.magickGetImageHeight w)

getImageWidth :: (MonadResource m) => Ptr MagickWand -> m Int
getImageWidth w = liftIO $ fmap fromIntegral (F.magickGetImageWidth w)

resizeImage :: (MonadResource m) => Ptr MagickWand -> Int -> Int -> FilterTypes -> CDouble -> m Bool
resizeImage pw w h f s = liftIO $ F.magickResizeImage pw (fromIntegral w) (fromIntegral h) f s >>= return . (==mTrue)

getImageCompressionQuality :: (MonadResource m) => Ptr MagickWand -> m Int
getImageCompressionQuality = liftIO . fmap fromIntegral . F.magickGetImageCompressionQuality

setImageCompressionQuality :: (MonadResource m) => Ptr MagickWand -> Int -> m Bool
setImageCompressionQuality w s = liftIO $ fmap (==mTrue) (F.magickSetImageCompressionQuality w (fromIntegral s))

getImageBackgroundColor :: (MonadResource m) => PMagickWand -> m (Maybe PPixelWand) -- TODO: use ErrorT
getImageBackgroundColor w = pixelWand >>= \p -> getImageBackgroundColor1 w p >>= \x -> return $! if x then Just p else Nothing

getImageBackgroundColor1 :: (MonadResource m) => PMagickWand -> PPixelWand -> m (Bool)
getImageBackgroundColor1 w p = (liftIO $ F.magickGetImageBackgroundColor w p) >>= return . (==mTrue)

setImageBackgroundColor :: (MonadResource m) => PMagickWand -> PPixelWand -> m (Bool)
setImageBackgroundColor w p = (liftIO $ F.magickSetImageBackgroundColor w p) >>= return . (==mTrue)

extentImage :: (MonadResource m) => PMagickWand -> Int -> Int -> Int -> Int -> m ()
extentImage w width height offsetX offsetY = 
  liftIO $ F.magickExtentImage w (fromIntegral width) (fromIntegral height) (fromIntegral offsetX) (fromIntegral offsetY)
