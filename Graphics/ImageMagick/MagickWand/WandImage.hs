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
  , newImage
  , drawImage
  , borderImage
  , shaveImage
  , setImageAlphaChannel
  , flipImage
  , flopImage
  , blurImage
  , blurImageChannel
  , normalizeImage
  , normalizeImageChannel
  , shadowImage
  , addImage
  , appendImages
  , addNoiseImage
  , writeImage
  , writeImages
  , setVirtualPixelMethod
  , trimImage
  , resetImagePage
  , distortImage
  , shadeImage
  , colorizeImage
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                                (ByteString, useAsCString)
import           Filesystem.Path.CurrentOS
import           Foreign
import           Foreign.C.Types
import           Graphics.ImageMagick.MagickCore.FFI.Distort
import           Graphics.ImageMagick.MagickCore.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.MagickWand as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.WandImage  as F
import           Graphics.ImageMagick.MagickWand.PixelWand
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils
import           Prelude                                        hiding (FilePath)

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

-- | newImage adds a blank image canvas of the specified size and background color to the wand.
newImage :: (MonadResource m)
  => PMagickWand
  -> Int               -- ^ width
  -> Int               -- ^ height
  -> PPixelWand        -- ^ background color
  -> m ()
newImage p width height b = withException_ p $! F.magickNewImage p (fromIntegral width) (fromIntegral height) b

-- |  drawImage renders the drawing wand on the current image.
drawImage :: (MonadResource m) => PMagickWand -> PDrawingWand -> m ()
drawImage p d = withException_ p $ F.magickDrawImage p d

borderImage :: (MonadResource m) => PMagickWand -> PPixelWand -> Int -> Int -> m ()
borderImage w bordercolor height width = withException_ w $ F.magickBorderImage w bordercolor (fromIntegral width) (fromIntegral height)

shaveImage :: (MonadResource m) => PMagickWand -> Int -> Int -> m ()
shaveImage w columns rows = withException_ w $ F.magickShaveImage w (fromIntegral columns) (fromIntegral rows)

setImageAlphaChannel :: (MonadResource m) => PMagickWand -> AlphaChannelType -> m ()
setImageAlphaChannel w alpha_type = withException_ w $ F.magickSetImageAlphaChannel w alpha_type

flipImage :: (MonadResource m) => Ptr MagickWand -> m ()
flipImage w = withException_ w $ F.magickFlipImage w

flopImage :: (MonadResource m) => Ptr MagickWand -> m ()
flopImage w = withException_ w $ F.magickFlopImage w

addImage :: (MonadResource m) => PMagickWand -> PMagickWand -> m ()
addImage w w' = withException_ w $ F.magickAddImage w w'

-- | MagickAppendImages() append the images in a wand from the current image onwards,
-- creating a new wand with the single image result. This is affected by the gravity
-- and background settings of the first image.
-- Typically you would call either MagickResetIterator() or MagickSetFirstImage() before
-- calling this function to ensure that all the images in the wand's image list will be appended together.
appendImages :: (MonadResource m)
             => PMagickWand
             -> Bool            -- ^ By default, images are stacked left-to-right. Set stack to MagickTrue to stack them top-to-bottom.
             -> m (ReleaseKey, PMagickWand)
appendImages w b = allocate (F.magickAppendImages w (toMBool b)) (void . F.destroyMagickWand)

-- |  MagickAddNoiseImage() adds random noise to the image.
--
addNoiseImage :: (MonadResource m)
              => PMagickWand
              -> NoiseType -- ^ The type of noise: Uniform, Gaussian, Multiplicative, Impulse, Laplacian, or Poisson.
              -> m ()
addNoiseImage w n = withException_ w $ F.magickAddNoiseImage w n

-- | writeImage() writes an image to the specified filename. If the filename
-- parameter is Nothing, the image is written to the filename set by MagickReadImage
-- or MagickSetImageFilename().
writeImage :: (MonadResource m)
           => PMagickWand
           -> Maybe (FilePath)
           -> m ()
writeImage w Nothing   = withException_ w $ F.magickWriteImage w nullPtr
writeImage w (Just fn) = withException_ w $ useAsCString (encode fn) (\f -> F.magickWriteImage w f)

writeImages :: (MonadResource m) => Ptr MagickWand -> FilePath -> Bool -> m ()
writeImages w fn b = withException_ w $ useAsCString (encode fn) (\f -> F.magickWriteImages w f (toMBool b))

-- | MagickBlurImage() blurs an image. We convolve the image with a gaussian
-- operator of the given radius and standard deviation (sigma). For reasonable
-- results, the radius should be larger than sigma. Use a radius of 0 and
-- BlurImage() selects a suitable radius for you.
--
-- The format of the MagickBlurImage method is:
blurImage :: (MonadResource m) => PMagickWand -> Double -> Double -> m ()
blurImage w r s = withException_ w $ F.magickBlurImage w (realToFrac r) (realToFrac s)

blurImageChannel :: (MonadResource m) => PMagickWand -> ChannelType -> Double -> Double -> m ()
blurImageChannel w c r s = withException_ w $ F.magickBlurImageChannel w c (realToFrac r) (realToFrac s)

-- | MagickNormalizeImage() enhances the contrast of a color image by adjusting
--   the pixels color to span the entire range of colors available
--
--   You can also reduce the influence of a particular channel with a gamma
--   value of 0.
normalizeImage :: (MonadResource m) => PMagickWand -> m ()
normalizeImage w = withException_ w $ F.magickNormalizeImage w

normalizeImageChannel :: (MonadResource m) => PMagickWand -> ChannelType -> m ()
normalizeImageChannel w c = withException_ w $ F.magickNormalizeImageChannel w c

-- | Simulates an image shadow.
shadowImage :: (MonadResource m)
  => PMagickWand  -- ^ the magick wand
  -> Double       -- ^ percentage transparency
  -> Double       -- ^ the standard deviation of the Gaussian, in pixels
  -> Int          -- ^ the shadow x-offset
  -> Int          -- ^ the shadow y-offset
  -> m ()
shadowImage w opacity sigma x y = withException_ w $ F.magickShadowImage w (realToFrac opacity) (realToFrac sigma)
                                                                         (fromIntegral x) (fromIntegral y)

-- | sets the image virtual pixel method.
--   the image virtual pixel method : UndefinedVirtualPixelMethod, ConstantVirtualPixelMethod,
--   EdgeVirtualPixelMethod, MirrorVirtualPixelMethod, or TileVirtualPixelMethod.
setVirtualPixelMethod :: (MonadResource m) => PMagickWand -> VirtualPixelMethod -> m VirtualPixelMethod
setVirtualPixelMethod = (liftIO .). F.magickSetVirtualPixelMethod

-- | Remove edges that are the background color from the image.
trimImage :: (MonadResource m) => PMagickWand -> Double -> m ()
trimImage w fuzz = withException_ w $ F.magickTrimImage w (realToFrac fuzz)

-- | Resets the Wand page canvas and position.
resetImagePage :: (MonadResource m) => PMagickWand -> ByteString -> m ()
resetImagePage w page = withException_ w $ useAsCString page (F.magickResetImagePage w)

-- | Resets the Wand page canvas and position.
distortImage :: (MonadResource m)
  => PMagickWand
  -> DistortImageMethod -- ^ the method of image distortion
  -> [Double]           -- ^ the arguments for this distortion method
  -> Bool               -- ^ attempt to resize destination to fit distorted source
  -> m ()
distortImage w method args bestfit = withException_ w $! withArrayLen (map realToFrac args) distort
  where
    distort len arr = F.magickDistortImage w method (fromIntegral len) arr (toMBool bestfit)

-- | Sshines a distant light on an image to create
-- a three-dimensional effect. You control the positioning of the light
-- with azimuth and elevation; azimuth is measured in degrees off the x axis
-- and elevation is measured in pixels above the Z axis.
shadeImage :: (MonadResource m)
  => PMagickWand
  -> Bool   -- ^ a value other than zero shades the intensity of each pixel
  -> Double -- ^ azimuth of the light source direction
  -> Double -- ^ evelation of the light source direction
  -> m ()
shadeImage w gray azimuth elevation = withException_ w $ F.magickShadeImage w (toMBool gray)
                                                                            (realToFrac azimuth) (realToFrac elevation)

-- | Resets the Wand page canvas and position.
colorizeImage :: (MonadResource m) => PMagickWand -> PPixelWand -> PPixelWand -> m ()
colorizeImage w colorize opacity = withException_ w $! F.magickColorizeImage w colorize opacity
