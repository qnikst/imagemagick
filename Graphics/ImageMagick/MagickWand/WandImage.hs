module Graphics.ImageMagick.MagickWand.WandImage
  ( getImageHeight
  , getImageWidth
  , resizeImage
  , getImageCompressionQuality
  , setImageCompressionQuality
  , getImageBackgroundColor
  , setImageBackgroundColor
  , extentImage
  , floodfillPaintImage
  , negateImage
  , negateImageChannel
  , getImageClipMask
  , setImageClipMask
  , compositeImage
  , compositeImageChannel
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Foreign
import           Foreign.C.Types
import           Graphics.ImageMagick.MagickCore.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.MagickWand as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.WandImage  as F
import           Graphics.ImageMagick.MagickWand.PixelWand
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils

getImageHeight :: (MonadResource m) => Ptr MagickWand -> m Int
getImageHeight w = liftIO $ fmap fromIntegral (F.magickGetImageHeight w)

getImageWidth :: (MonadResource m) => Ptr MagickWand -> m Int
getImageWidth w = liftIO $ fmap fromIntegral (F.magickGetImageWidth w)

resizeImage :: (MonadResource m) => Ptr MagickWand -> Int -> Int -> FilterTypes -> CDouble -> m Bool
resizeImage pw w h f s = fromMBool $! F.magickResizeImage pw (fromIntegral w) (fromIntegral h) f s

getImageCompressionQuality :: (MonadResource m) => Ptr MagickWand -> m Int
getImageCompressionQuality = liftIO . fmap fromIntegral . F.magickGetImageCompressionQuality

setImageCompressionQuality :: (MonadResource m) => Ptr MagickWand -> Int -> m Bool
setImageCompressionQuality w s = fromMBool $! F.magickSetImageCompressionQuality w (fromIntegral s)

getImageBackgroundColor :: (MonadResource m) => PMagickWand -> m (Maybe PPixelWand) -- TODO: use ErrorT
getImageBackgroundColor w = pixelWand >>= \p -> getImageBackgroundColor1 w p >>= \x -> return $! if x then Just p else Nothing

getImageBackgroundColor1 :: (MonadResource m) => PMagickWand -> PPixelWand -> m Bool
getImageBackgroundColor1 w p = fromMBool $! F.magickGetImageBackgroundColor w p

setImageBackgroundColor :: (MonadResource m) => PMagickWand -> PPixelWand -> m Bool
setImageBackgroundColor w p = fromMBool $! F.magickSetImageBackgroundColor w p

extentImage :: (MonadResource m) => PMagickWand -> Int -> Int -> Int -> Int -> m ()
extentImage w width height offsetX offsetY =
  liftIO $ F.magickExtentImage w (fromIntegral width) (fromIntegral height) (fromIntegral offsetX) (fromIntegral offsetY)

floodfillPaintImage :: (MonadResource m) => PMagickWand -> ChannelType -> PPixelWand -> Double -> PPixelWand -> Int -> Int -> Bool -> m Bool
floodfillPaintImage w channel fill fuzz border x y invert =
  fromMBool $! F.magickFloodfillPaintImage w channel fill (realToFrac fuzz) border (fromIntegral x) (fromIntegral y) (toMBool invert)

negateImage :: (MonadResource m) => PMagickWand -> Bool -> m Bool
negateImage p b = fromMBool $! F.magickNegateImage p (toMBool b)

negateImageChannel :: (MonadResource m) => PMagickWand -> ChannelType -> Bool -> m Bool
negateImageChannel p c b = fromMBool $! F.magickNegateImageChannel p c (toMBool b)


getImageClipMask :: (MonadResource m) => PMagickWand -> m PMagickWand
getImageClipMask = liftIO . F.magickGetImageClipMask

setImageClipMask :: (MonadResource m) => PMagickWand -> PMagickWand -> m Bool
setImageClipMask = (fromMBool .) . F.magickSetImageClipMask


compositeImage :: (MonadResource m) => PMagickWand -> PMagickWand -> CompositeOperator -> Int -> Int -> m Bool
compositeImage p s c w h = fromMBool $ F.magickCompositeImage p s c (fromIntegral w) (fromIntegral h)

compositeImageChannel :: (MonadResource m) => PMagickWand -> PMagickWand -> ChannelType -> CompositeOperator -> Int -> Int -> m Bool
compositeImageChannel p s ch c w h = fromMBool $ F.magickCompositeImageChannel p s ch c (fromIntegral w) (fromIntegral h)

