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
  , transparentPaintImage
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

resizeImage :: (MonadResource m) => Ptr MagickWand -> Int -> Int -> FilterTypes -> CDouble -> m ()  --TODO: fix CDouble
resizeImage pw w h f s = withException_ pw $! F.magickResizeImage pw (fromIntegral w) (fromIntegral h) f s

getImageCompressionQuality :: (MonadResource m) => Ptr MagickWand -> m Int
getImageCompressionQuality = liftIO . fmap fromIntegral . F.magickGetImageCompressionQuality

setImageCompressionQuality :: (MonadResource m) => Ptr MagickWand -> Int -> m () 
setImageCompressionQuality w s = withException_ w $! F.magickSetImageCompressionQuality w (fromIntegral s)

getImageBackgroundColor :: (MonadResource m) => PMagickWand -> m PPixelWand
getImageBackgroundColor w = pixelWand >>= \p -> getImageBackgroundColor1 w p >> return p 

getImageBackgroundColor1 :: (MonadResource m) => PMagickWand -> PPixelWand -> m ()
getImageBackgroundColor1 w p = withException_ w $! F.magickGetImageBackgroundColor w p

setImageBackgroundColor :: (MonadResource m) => PMagickWand -> PPixelWand -> m () 
setImageBackgroundColor w p = withException_ w $! F.magickSetImageBackgroundColor w p

extentImage :: (MonadResource m) => PMagickWand -> Int -> Int -> Int -> Int -> m ()
extentImage w width height offsetX offsetY = withException_ w $! 
  F.magickExtentImage w (fromIntegral width) (fromIntegral height) (fromIntegral offsetX) (fromIntegral offsetY)

floodfillPaintImage :: (MonadResource m) => PMagickWand -> ChannelType -> PPixelWand -> Double -> PPixelWand -> Int -> Int -> Bool -> m ()
floodfillPaintImage w channel fill fuzz border x y invert = withException_ w $!
  F.magickFloodfillPaintImage w channel fill (realToFrac fuzz) border (fromIntegral x) (fromIntegral y) (toMBool invert)

negateImage :: (MonadResource m) => PMagickWand -> Bool -> m ()
negateImage p b = withException_ p $! F.magickNegateImage p (toMBool b)

negateImageChannel :: (MonadResource m) => PMagickWand -> ChannelType -> Bool -> m ()
negateImageChannel p c b = withException_ p $! F.magickNegateImageChannel p c (toMBool b)

getImageClipMask :: (MonadResource m) => PMagickWand -> m PMagickWand
getImageClipMask = liftIO . F.magickGetImageClipMask

setImageClipMask :: (MonadResource m) => PMagickWand -> PMagickWand -> m ()
setImageClipMask w s = withException_ w $ F.magickSetImageClipMask w s

compositeImage :: (MonadResource m) => PMagickWand -> PMagickWand -> CompositeOperator -> Int -> Int -> m () 
compositeImage p s c w h = withException_ p $ F.magickCompositeImage p s c (fromIntegral w) (fromIntegral h)

compositeImageChannel :: (MonadResource m) => PMagickWand -> PMagickWand -> ChannelType -> CompositeOperator -> Int -> Int -> m ()
compositeImageChannel p s ch c w h = withException_ p $ 
  F.magickCompositeImageChannel p s ch c (fromIntegral w) (fromIntegral h)

-- | transparentPaintImage changes any pixel that matches color with the color defined by fill.
transparentPaintImage :: (MonadResource m) 
  => PMagickWand
  -> PPixelWand           -- ^ change this color to specified opacity value withing the image
  -> Double               -- ^ the level of transarency: 1.0 fully opaque 0.0 fully transparent
  -> Double               -- ^ By default target must match a particular pixel color exactly. 
                          -- However, in many cases two colors may differ by a small amount. 
                          -- The fuzz member of image defines how much tolerance is acceptable 
                          -- to consider two colors as the same. For example, set fuzz to 10 and 
                          -- the color red at intensities of 100 and 102 respectively are now 
                          -- interpreted as the same color for the purposes of the floodfill.
  -> Bool                 -- paint any pixel that does not match the target color.
  -> m () 
transparentPaintImage w p alfa fuzz invert = withException_ w $ F.magickTransparentPaintImage w p alfa fuzz (toMBool invert) 
