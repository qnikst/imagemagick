{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.ImageMagick.MagickWand.WandImage
  ( getImageHeight
  , getImageWidth
  , getImagePixelColor
  , resizeImage
  , getImageCompressionQuality
  , setImageCompression
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
  , fxImage
  , fxImageChannel
  , sigmoidalContrastImage
  , sigmoidalContrastImageChannel
  , evaluateImage
  , evaluateImageChannel
  , evaluateImages
  , rollImage
  , annotateImage
  , mergeImageLayers
  , tintImage
  , gaussianBlurImageChannel
  , gaussianBlurImage
  , setImageMatte
  , cropImage
  , shearImage
  , scaleImage
  , sparseColorImage
  , functionImage
  , functionImageChannel
  , coalesceImages
  , getNumberImages
  , getImage
  , compareImageLayers
-- , getImageFromMagickWand
-- , adaptiveBlurImage
-- , adaptiveResizeImage
-- , adaptiveSharpenImage
-- , adaptiveThresholdImage
-- , addImage
-- , addNoiseImage
-- , affineTransformImage
-- , annotateImage
-- , animateImages
-- , appendImages
-- , autoGammaImage
-- , autoLevelImage
-- , blackThresholdImage
-- , blueShiftImage
-- , blurImage
-- , borderImage
-- , brightnessContrastImage
-- , charcoalImage
-- , chopImage
-- , clampImage
-- , clipImage
-- , clipImagePath
-- , clutImage
-- , coalesceImages
-- , colorDecisionListImage
-- , colorizeImage
-- , colorMatrixImage
-- , combineImages
-- , commentImage
-- , compareImageChannels
-- , compareImageLayers
-- , compareImages
-- , compositeImage
-- , compositeLayers
-- , contrastImage
-- , contrastStretchImage
-- , convolveImage
-- , cropImage
-- , cycleColormapImage
-- , constituteImage
-- , decipherImage
-- , deconstructImages
-- , deskewImage
-- , despeckleImage
-- , destroyImage
-- , displayImage
-- , displayImages
-- , distortImage
-- , drawImage
-- , edgeImage
-- , embossImage
-- , encipherImage
-- , enhanceImage
-- , equalizeImage
-- , evaluateImage
-- , exportImagePixels
-- , extentImage
-- , filterImage
-- , flipImage
-- , floodfillPaintImage
-- , flopImage
-- , forwardFourierTransformImage
-- , frameImage
-- , functionImage
-- , fxImage
-- , gammaImage
-- , gaussianBlurImage
-- , getImage
  , getImageAlphaChannel
-- , getImageClipMask
-- , getImageBackgroundColor
  , getImageBlob
-- , getImageBluePrimary
-- , getImageBorderColor
-- , getImageChannelDepth
-- , getImageChannelDistortion
-- , getImageChannelDistortions
-- , getImageChannelFeatures
-- , getImageChannelKurtosis
-- , getImageChannelMean
-- , getImageChannelRange
-- , getImageChannelStatistics
-- , getImageColormapColor
-- , getImageColors
-- , getImageColorspace
-- , getImageCompose
-- , getImageCompression
-- , getImageCompressionQuality
  , getImageDelay
  , getImageDepth
-- , getImageDistortion
-- , getImageDispose
-- , getImageEndian
-- , getImageFilename
  , getImageFormat
-- , getImageFuzz
-- , getImageGamma
-- , getImageGravity
-- , getImageGreenPrimary
-- , getImageHeight
-- , getImageHistogram
-- , getImageInterlaceScheme
-- , getImageInterpolateMethod
-- , getImageIterations
-- , getImageLength
-- , getImageMatteColor
-- , getImageOrientation
-- , getImagePage
-- , getImagePixelColor
-- , getImageRedPrimary
-- , getImageRegion
-- , getImageRenderingIntent
  , getImageSignature
-- , getImageTicksPerSecond
-- , getImageType
-- , getImageUnits
-- , getImageVirtualPixelMethod
-- , getImageWhitePoint
-- , getImageWidth
-- , getNumberImages
-- , getImageTotalInkDensity
-- , haldClutImage
-- , hasNextImage
-- , hasPreviousImage
-- , identifyImage
-- , implodeImage
-- , importImagePixels
-- , inverseFourierTransformImage
-- , labelImage
-- , levelImage
-- , linearStretchImage
-- , liquidRescaleImage
-- , magnifyImage
-- , mergeImageLayers
-- , minifyImage
-- , modulateImage
-- , montageImage
-- , morphImages
-- , morphologyImage
-- , motionBlurImage
-- , negateImage
-- , newImage
-- , nextImage
-- , normalizeImage
-- , oilPaintImage
-- , opaquePaintImage
-- , optimizeImageLayers
-- , optimizeImageTransparency
-- , orderedPosterizeImage
-- , pingImage
-- , pingImageBlob
-- , pingImageFile
-- , polaroidImage
-- , posterizeImage
-- , previewImages
-- , previousImage
-- , quantizeImage
-- , quantizeImages
-- , radialBlurImage
-- , raiseImage
-- , randomThresholdImage
  , readImage
  , readImageBlob
-- , readImageFile
-- , remapImage
-- , resampleImage
-- , resetImagePage
-- , resizeImage
-- , rollImage
-- , rotateImage
-- , sampleImage
-- , scaleImage
-- , segmentImage
-- , selectiveBlurImage
-- , separateImageChannel
-- , sepiaToneImage
-- , setImage
-- , setImageAlphaChannel
-- , setImageBackgroundColor
-- , setImageBias
-- , setImageBluePrimary
-- , setImageBorderColor
-- , setImageChannelDepth
-- , setImageClipMask
-- , setImageColor
-- , setImageColormapColor
-- , setImageColorspace
-- , setImageCompose
-- , setImageCompression
-- , setImageCompressionQuality
  , setImageDelay
  , setImageDepth
-- , setImageDispose
-- , setImageEndian
-- , setImageExtent
-- , setImageFilename
  , setImageFormat
-- , setImageFuzz
-- , setImageGamma
-- , setImageGravity
-- , setImageGreenPrimary
-- , setImageInterlaceScheme
-- , setImageInterpolateMethod
-- , setImageIterations
-- , setImageMatte
-- , setImageMatteColor
-- , setImageOpacity
-- , setImageOrientation
-- , setImagePage
-- , setImageProgressMonitor
-- , setImageRedPrimary
-- , setImageRenderingIntent
-- , setImageScene
-- , setImageTicksPerSecond
  , setImageType
-- , setImageUnits
-- , setImageVirtualPixelMethod
-- , setImageWhitePoint
-- , shadeImage
-- , shadowImage
-- , sharpenImage
-- , shaveImage
-- , shearImage
-- , sigmoidalContrastImage
-- , similarityImage
-- , sketchImage
-- , smushImages
-- , solarizeImage
-- , sparseColorImage
-- , spliceImage
-- , spreadImage
-- , statisticImage
-- , steganoImage
-- , stereoImage
  , stripImage
-- , swirlImage
-- , textureImage
-- , thresholdImage
-- , thumbnailImage
-- , tintImage
-- , transformImage
-- , transformImageColorspace
-- , transparentPaintImage
-- , transposeImage
-- , transverseImage
-- , trimImage
-- , uniqueImageColors
-- , unsharpMaskImage
-- , vignetteImage
-- , waveImage
-- , whiteThresholdImage
-- , writeImage
-- , writeImageFile
-- , writeImages
-- , writeImagesFile
  , getImageScene
  , setImage
  , removeImage
  , importImagePixels
  , exportImagePixels
  , rotateImage
  ) where

import           Control.Applicative                            ((<$>))
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                                (ByteString,
                                                                 packCString,
                                                                 packCStringLen,
                                                                 useAsCString,
                                                                 useAsCStringLen)
import           Data.Text                                      (Text)
import qualified Data.Text                                      as T
import           Data.Text.Encoding                             (decodeUtf8,
                                                                 encodeUtf8)
import           Data.Vector.Storable                           (Vector)
import qualified Data.Vector.Storable                           as V
import           Filesystem.Path.CurrentOS
import           Foreign hiding (void)
import           Foreign.C.Types
import           Graphics.ImageMagick.MagickCore.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.MagickWand as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.WandImage  as F
import           Graphics.ImageMagick.MagickWand.MagickWand
import           Graphics.ImageMagick.MagickWand.PixelWand
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils
import           Prelude                                        hiding
                                                                 (FilePath)

getImageHeight :: (MonadResource m) => Ptr MagickWand -> m Int
getImageHeight w = liftIO $ fmap fromIntegral (F.magickGetImageHeight w)

getImageWidth :: (MonadResource m) => Ptr MagickWand -> m Int
getImageWidth w = liftIO $ fmap fromIntegral (F.magickGetImageWidth w)

-- | returns the color of the specified pixel into the pixelwand.
getImagePixelColor :: (MonadResource m)
  => PMagickWand
  -> Int          -- ^ pixel x coordinate
  -> Int          -- ^ pixel y coordinate
  -> PPixelWand   -- ^ return the colormap color in this wand
  -> m ()
getImagePixelColor w x y pw = withException_ w $! F.magickGetImagePixelColor w (fromIntegral x) (fromIntegral y) pw

resizeImage :: (MonadResource m) => Ptr MagickWand -> Int -> Int -> FilterTypes -> Double -> m ()
resizeImage pw w h f s = withException_ pw $! F.magickResizeImage pw (fromIntegral w) (fromIntegral h) f (realToFrac s)

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
resetImagePage :: (MonadResource m) => PMagickWand -> Maybe Text -> m ()
resetImagePage w Nothing = withException_ w $ F.magickResetImagePage w nullPtr
resetImagePage w (Just page) = withException_ w $ useAsCString (encodeUtf8 page) (F.magickResetImagePage w)

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

-- | Evaluate expression for each pixel in the image.
fxImage :: (MonadResource m) => PMagickWand -> Text -> m (ReleaseKey, Ptr MagickWand)
fxImage w expr = wandResource (useAsCString (encodeUtf8 expr) (F.magickFxImage w))

-- | Evaluate expression for each pixel in the image.
fxImageChannel :: (MonadResource m) => PMagickWand -> ChannelType -> Text -> m (ReleaseKey, Ptr MagickWand)
fxImageChannel w channel expr = wandResource (useAsCString (encodeUtf8 expr) (F.magickFxImageChannel w channel))

-- | Adjusts the contrast of an image with a  non-linear sigmoidal contrast algorithm.
-- Increase the contrast of the image using a sigmoidal transfer function without
-- saturating highlights or shadows. Contrast indicates how much to increase the contrast
-- (0 is none; 3 is typical; 20 is pushing it); mid-point indicates where midtones fall
-- in the resultant image (0 is white; 50 is middle-gray; 100 is black). Set sharpen to `True`
-- to increase the image contrast otherwise the contrast is reduced.
sigmoidalContrastImage :: (MonadResource m) => PMagickWand -> Bool -> Double -> Double -> m ()
sigmoidalContrastImage w sharpen alpha beta =
  withException_ w $! F.magickSigmoidalContrastImage w (toMBool sharpen) (realToFrac alpha) (realToFrac beta)

-- see `sigmoidalContrastImage`
sigmoidalContrastImageChannel :: (MonadResource m) => PMagickWand -> ChannelType -> Bool -> Double -> Double -> m ()
sigmoidalContrastImageChannel w channel sharpen alpha beta =
  withException_ w $! F.magickSigmoidalContrastImageChannel w channel (toMBool sharpen) (realToFrac alpha) (realToFrac beta)

-- | Applies an arithmetic, relational, or logical expression to an image.
-- Use these operators to lighten or darken an image, to increase or decrease
-- contrast in an image, or to produce the "negative" of an image.
evaluateImage :: (MonadResource m)
  => PMagickWand
  -> MagickEvaluateOperator -- ^ a channel operator
  -> CDouble                -- ^ value
  -> m ()
evaluateImage w op value = withException_ w $! F.magickEvaluateImage w op value

-- | see `evaluateImage`
evaluateImages :: (MonadResource m)
  => PMagickWand
  -> MagickEvaluateOperator -- ^ a channel operator
  -> m ()
evaluateImages w op = withException_ w $! F.magickEvaluateImages w op

-- | see `evaluateImage`
evaluateImageChannel :: (MonadResource m)
  => PMagickWand
  -> ChannelType            -- ^ the channel(s)
  -> MagickEvaluateOperator -- ^ a channel operator
  -> CDouble                -- ^ value
  -> m ()
evaluateImageChannel w channel op value = withException_ w $! F.magickEvaluateImageChannel w channel op value

-- | Offsets an image as defined by x and y.
rollImage :: (MonadResource m) => PMagickWand -> Double -> Double -> m ()
rollImage w x y = withException_ w $! F.magickRollImage w (realToFrac x) (realToFrac y)

-- | Annotates an image with text.
annotateImage :: (MonadResource m)
  => PMagickWand
  -> PDrawingWand -- ^ the draw wand
  -> Double       -- ^ x ordinate to left of text
  -> Double       -- ^ y ordinate to text baseline
  -> Double       -- ^ rotate text relative to this angle
  -> Text         -- ^ text to draw
  -> m ()
annotateImage w dw x y angle text =
  withException_ w $! useAsCString (encodeUtf8 text)
                                   (F.magickAnnotateImage w dw (realToFrac x) (realToFrac y) (realToFrac angle))

-- | Composes all the image layers from the current given image onward to
-- produce a single image of the merged layers. The inital canvas's size
-- depends on the given ImageLayerMethod, and is initialized using the first
-- images background color. The images are then compositied onto that image
-- in sequence using the given composition that has been assigned to each
-- individual image.
mergeImageLayers :: (MonadResource m) => PMagickWand -> ImageLayerMethod -> m (ReleaseKey, PMagickWand)
mergeImageLayers w method = wandResource (F.magickMergeImageLayers w method)

-- | Applies a color vector to each pixel in the image. The length of the
-- vector is 0 for black and white and at its maximum for the midtones.
-- The vector weighting function is f(x)=(1-(4.0*((x-0.5)*(x-0.5)))).
--
-- The format of the MagickTintImage method is:
tintImage :: (MonadResource m) => PMagickWand
          -> PPixelWand    -- ^ tint pixel
          -> PPixelWand    -- ^ opacity pixel
          -> m ()
tintImage w t o = withException_ w $ F.magickTintImage w t o


-- |  MagickGaussianBlurImage() blurs an image. We convolve the image with a Gaussian operator
-- of the given radius and standard deviation (sigma). For reasonable results, the radius should
-- be larger than sigma. Use a radius of 0 and MagickGaussianBlurImage() selects a suitable radius for you.
gaussianBlurImage :: (MonadResource m) => PMagickWand
                  -> Double
                  -> Double
                  -> m ()
gaussianBlurImage w r s = withException_ w $ F.magickGaussianBlurImage w (realToFrac r) (realToFrac s)

gaussianBlurImageChannel :: (MonadResource m) => PMagickWand
                  -> ChannelType
                  -> Double
                  -> Double
                  -> m ()
gaussianBlurImageChannel w c r s = withException_ w $ F.magickGaussianBlurImageChannel w c (realToFrac r) (realToFrac s)

setImageMatte :: (MonadResource m) => PMagickWand
              -> Bool
              -> m ()
setImageMatte w b = withException_ w $ F.magickSetImageMatte w (toMBool b)

-- | Extracts a region of the image.
cropImage :: (MonadResource m) => PMagickWand
  -> Int         -- ^ the region width
  -> Int         -- ^ the region height
  -> Int         -- ^ the region x-offset
  -> Int         -- ^ the region y-offset
  -> m ()
cropImage w width height x y = withException_ w $ F.magickCropImage w (fromIntegral width) (fromIntegral height)
                                                                      (fromIntegral x) (fromIntegral y)

-- | Slides one edge of an image along the X or Y axis, creating
-- a parallelogram. An X direction shear slides an edge along
-- the X axis, while a Y direction shear slides an edge along
-- the Y axis. The amount of the shear is controlled by a shear
-- angle. For X direction shears, x_shear is measured relative
-- to the Y axis, and similarly, for Y direction shears y_shear is
-- measured relative to the X axis. Empty triangles left over from
-- shearing the image are filled with the background color.
shearImage :: (MonadResource m) => PMagickWand
  -> PPixelWand -- ^ the background pixel wand
  -> Double     -- ^ the number of degrees to shear the image
  -> Double     -- ^ the number of degrees to shear the image
  -> m ()
shearImage w pw x_shear y_shear =
  withException_ w $ F.magickShearImage w pw (realToFrac x_shear) (realToFrac y_shear)

-- | Scales the size of an image to the given dimensions.
scaleImage :: (MonadResource m) => PMagickWand
  -> Int        -- ^ the number of degrees to shear the image
  -> Int        -- ^ the number of degrees to shear the image
  -> m ()
scaleImage w columns rows =
  withException_ w $ F.magickScaleImage w (fromIntegral columns) (fromIntegral rows)


-- | MagickSparseColorImage(), given a set of coordinates, interpolates the
-- colors found at those coordinates, across the whole image, using various methods.
--
-- The format of the MagickSparseColorImage method is:
--   ArcSparseColorion will always ignore source image offset, and always 'bestfit'
-- the destination image with the top left corner offset relative to the polar mapping center.
--
-- Bilinear has no simple inverse mapping so will not allow 'bestfit' style of image sparseion.
--
-- Affine, Perspective, and Bilinear, will do least squares fitting of the distrotion when more
-- than the minimum number of control point pairs are provided.
--
-- Perspective, and Bilinear, will fall back to a Affine sparseion when less than 4 control
-- point pairs are provided. While Affine sparseions will let you use any number of control
-- point pairs, that is Zero pairs is a No-Op (viewport only) distrotion, one pair is a
-- translation and two pairs of control points will do a scale-rotate-translate, without any
-- shearing.
sparseColorImage :: (MonadResource m) => PMagickWand
                 -> ChannelType
                 -> SparseColorMethod
                 -> Vector Double
                 -> m()
sparseColorImage w c m v =
  withException_ w $ V.unsafeWith v $ \v' -> F.magickSparseColorImage w c m (fromIntegral $ V.length v) v'

-- | MagickFunctionImage() applys an arithmetic, relational, or logical expression to an image.
-- Use these operators to lighten or darken an image, to increase or decrease contrast in an
-- image, or to produce the "negative" of an image.
functionImage :: (MonadResource m) => PMagickWand -> MagickFunction -> Vector Double -> m ()
functionImage w f v =
  withException_ w $ V.unsafeWith v $ \v' -> F.magickFunctionImage w f (fromIntegral $ V.length v) v'

functionImageChannel :: (MonadResource m) => PMagickWand -> ChannelType -> MagickFunction -> Vector Double -> m ()
functionImageChannel w c f v =
  withException_ w $ V.unsafeWith v $ \v' -> F.magickFunctionImageChannel w c f (fromIntegral $ V.length v) v'


-- | MagickCoalesceImages() composites a set of images while respecting any page offsets and disposal methods. GIF, MIFF, and MNG animation sequences typically start with an image background and each subsequent image varies in size and offset. MagickCoalesceImages() returns a new sequence where each image in the sequence is the same size as the first and composited with the next image in the sequence.
coalesceImages :: (MonadResource m) => PMagickWand
  -> m (ReleaseKey, PMagickWand)
coalesceImages = wandResource . F.magickCoalesceImages

-- | returns the number of images associated with a magick wand.
getNumberImages :: (MonadResource m) => PMagickWand -> m Int
getNumberImages w = liftIO $ fromIntegral <$> F.magickGetNumberImages w

-- | Gets the image at the current image index.
getImage :: (MonadResource m) => PMagickWand -> m (ReleaseKey, PMagickWand)
getImage = wandResource . F.magickGetImage

-- | Compares each image with the next in a sequence and returns
-- the maximum bounding region of any pixel differences it discovers.
compareImageLayers :: (MonadResource m) => PMagickWand -> ImageLayerMethod -> m (ReleaseKey, PMagickWand)
compareImageLayers = (wandResource .). F.magickCompareImageLayers

-- | Gets the image scene
getImageScene :: (MonadResource m) => PMagickWand -> m Int
getImageScene w = liftIO $ fromIntegral <$> F.magickGetImageScene w

-- | MagickRemoveImage() removes an image from the image list.
removeImage :: (MonadResource m) => PMagickWand -> m ()
removeImage w = withException_ w $ F.magickRemoveImage w

-- | Replaces the last image returned by `setImageIndex` and
-- iteration methods with the images from the specified wand.
setImage :: (MonadResource m) => PMagickWand -> PMagickWand -> m ()
setImage w sw = withException_ w $ F.magickSetImage w sw

-- | Accepts pixel data. The pixel data can be in any `Pixels` format
-- in the order specified by map.
importImagePixels :: (MonadResource m, Pixel a) => PMagickWand
                     -> Int      -- ^ x
                     -> Int      -- ^ y
                     -> Int      -- ^ columns
                     -> Int      -- ^ rows
                     -- TODO migrate to typesafe parameter
                     -> Text     -- ^ map
                     -> [a]      -- ^ imported pixels
                     -> m ()
importImagePixels w x y width height cmap pixels =
  withException_ w $ useAsCString (encodeUtf8 cmap) $ \cstr ->
    withPixels pixels $ (F.magickImportImagePixels w x' y' width' height' cstr stype) . castPtr
    where
      x' = fromIntegral x
      y' = fromIntegral y
      width' = fromIntegral width
      height' = fromIntegral height
      stype   = pixelStorageType pixels

-- | Extracts pixel data from an image and returns it to you. The data is
-- returned as `Pixels` in the order specified by cmap.
exportImagePixels :: (MonadResource m, Pixel a) => PMagickWand
                     -> Int     -- ^ x
                     -> Int     -- ^ y
                     -> Int     -- ^ columns
                     -> Int     -- ^ rows
                     -- TODO migrate to typesafe parameter
                     -> Text    -- ^ map
                     -> m [a]
exportImagePixels w x y width height cmap = liftIO $ useAsCString (encodeUtf8 cmap) $  \cstr ->
  exportArray arrLength (F.magickExportImagePixels w x' y' width' height' cstr) (undefined)
  where
    exportArray :: (Pixel a) => Int -> (StorageType -> Ptr () -> IO b) -> [a] -> IO [a]
    exportArray s f hack = allocaArray s (\q -> f storage (castPtr q) >> peekArray s q)
      where storage = pixelStorageType hack
    x' = fromIntegral x
    y' = fromIntegral y
    width' = fromIntegral width
    height' = fromIntegral height
    arrLength = width * height * (T.length cmap)

-- | Rotates an image the specified number of degrees. Empty triangles left over
-- from rotating the image are filled with the background color.
rotateImage :: (MonadResource m) => PMagickWand -> PPixelWand -> Double -> m ()
rotateImage w background degrees = withException_ w $ F.magickRotateImage w background (realToFrac degrees)

-- | Gets the image depth.
getImageDepth :: (MonadResource m) => PMagickWand -> m Int
getImageDepth w = liftIO $ fromIntegral <$> F.magickGetImageDepth w

-- | Sets the image depth.
setImageDepth :: (MonadResource m) => PMagickWand -> Int -> m ()
setImageDepth w depth = withException_ w $ F.magickSetImageDepth w (fromIntegral depth)

-- | Sets the image compression.
setImageCompression:: (MonadResource m) => PMagickWand -> CompressionType -> m ()
setImageCompression w compressionType = withException_ w $ F.magickSetImageCompression w compressionType

-- | Gets the image delay.
getImageDelay :: (MonadResource m) => PMagickWand -> m Int
getImageDelay w = liftIO $ fromIntegral <$> F.magickGetImageDelay w

-- | Sets the image delay.
setImageDelay :: (MonadResource m) => PMagickWand -> Int -> m ()
setImageDelay w delay = withException_ w $ F.magickSetImageDelay w (fromIntegral delay)

-- | MagickGetImageBlob() implements direct to memory image formats.
-- It returns the image as a blob (a formatted "file" in memory) and
-- its length, starting from the current position in the image sequence.
-- Use 'setImageFormat' to set the format to write to the blob (GIF, JPEG, PNG, etc.).
-- ImageMagick blob is automatically freed in this function, returned bytestring
-- is on haskell heap.
getImageBlob :: (MonadResource m) => PMagickWand -> m ByteString
getImageBlob w = liftIO $ do
  F.magickResetIterator w
  cl <- alloca $ \x -> do
          c <- F.magickGetImageBlob w x
          x' <- fmap fromIntegral (peek x)
          return (c,x')
  out <- packCStringLen cl
  F.magickRelinquishMemory $ castPtr $ fst cl
  return out

-- | Reads an image or image sequence. The images are inserted at
-- the current image pointer position
readImage :: (MonadResource m) => Ptr MagickWand -> FilePath -> m ()
readImage w fn = withException_ w $ useAsCString (encode fn) (F.magickReadImage w)

-- | Reads an image or image sequence from a blob
readImageBlob :: (MonadResource m) => PMagickWand -> ByteString -> m ()
readImageBlob w bs = withException_ w $ useAsCStringLen bs $
                     \(cstr, len) -> F.magickReadImageBlob w (castPtr cstr) (fromIntegral len)

-- | Returns the format of a particular image in a sequence.
getImageFormat :: (MonadResource m) => PMagickWand -> m Text
getImageFormat w = liftIO $ do
                   cstr <- F.magickGetImageFormat w
                   -- TODO: check if we need to release string memory
                   decodeUtf8 <$> packCString cstr

setImageFormat :: (MonadResource m) => PMagickWand -> Text -> m ()
setImageFormat w format = withException_ w $ useAsCString (encodeUtf8 format) (F.magickSetImageFormat w)

-- | Strips an image of all profiles and comments.
stripImage :: (MonadResource m) => PMagickWand -> m ()
stripImage w = withException_ w $ (F.magickStripImage w)

-- | Generates an SHA-256 message digest for the image pixel stream
getImageSignature :: (MonadResource m) => PMagickWand -> m ByteString
getImageSignature w = liftIO $ F.magickGetImageSignature w >>= packCString

-- | Returns `False` if the image alpha channel is not activated.
-- That is, the image is RGB rather than RGBA or CMYK rather than CMYKA.
getImageAlphaChannel :: (MonadResource m) => PMagickWand -> m Bool
getImageAlphaChannel = fromMBool . F.magickGetImageAlphaChannel

-- | Sets image Type
setImageType :: (MonadResource m) => PMagickWand -> ImageType -> m ()
setImageType w imageType = withException_ w $ F.magickSetImageType w imageType
