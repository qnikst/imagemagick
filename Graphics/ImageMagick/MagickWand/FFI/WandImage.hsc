{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickWand.FFI.WandImage
  where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.Types

#include <wand/MagickWand.h>

-- | MagickGetImageHeight() returns the image height.
foreign import ccall "MagickGetImageHeight" magickGetImageHeight
  :: Ptr MagickWand -> IO CSize

-- | MagickGetImageWidth() returns the image width.
foreign import ccall "MagickGetImageWidth" magickGetImageWidth
  :: Ptr MagickWand -> IO CSize

-- |  MagickGetImageCompressionQuality() gets the image compression quality.

foreign import ccall "MagickGetImageCompressionQuality" magickGetImageCompressionQuality
  :: Ptr MagickWand -> IO CSize

-- | MagickSetImageCompressionQuality() sets the image compression quality.
foreign import ccall "MagickSetImageCompressionQuality" magickSetImageCompressionQuality
  :: Ptr MagickWand -> CSize -> IO MagickBooleanType

-- | MagickGetImageBackgroundColor() returns the image background color.
foreign import ccall "MagickGetImageBackgroundColor" magickGetImageBackgroundColor
  :: Ptr MagickWand -> Ptr PixelWand -> IO MagickBooleanType

-- | MagickSetImageBackgroundColor() sets the image background color.
foreign import ccall "MagickSetImageBackgroundColor" magickSetImageBackgroundColor
  :: Ptr MagickWand -> Ptr PixelWand -> IO MagickBooleanType

-- | MagickExtentImage() extends the image as defined by the geometry, gravity,
-- and wand background color. Set the (x,y) offset of the geometry to move the
-- original wand relative to the extended wand.
foreign import ccall "MagickExtentImage" magickExtentImage
  :: Ptr MagickWand  -- ^ wand
  -> CSize           -- ^ width
  -> CSize           -- ^ height
  -> CSize           -- ^ x offset
  -> CSize           -- ^ y offset
  -> IO MagickBooleanType

-- | MagickFloodfillPaintImage() changes the color value of any pixel
-- that matches target and is an immediate neighbor. If the method FillToBorderMethod
-- is specified, the color value is changed for any neighbor pixel that does
-- not match the bordercolor member of image.
foreign import ccall "MagickFloodfillPaintImage" magickFloodfillPaintImage
  :: Ptr MagickWand    -- ^ wand
  -> ChannelType       -- ^ channel
  -> Ptr PixelWand     -- ^ fill
  -> CDouble           -- ^ fuzz
  -> Ptr PixelWand     -- ^ bordercolor
  -> CSize             -- ^ x offset
  -> CSize             -- ^ y offset
  -> MagickBooleanType -- ^ invert
  -> IO MagickBooleanType

-- | MagickNegateImage() negates the colors in the reference image.
-- The Grayscale option means that only grayscale values within the image are negated.
-- You can also reduce the influence of a particular channel with a gamma value of 0.
foreign import ccall "MagickNegateImage" magickNegateImage
  :: Ptr MagickWand
  -> MagickBooleanType      -- ^ negate gray
  -> IO MagickBooleanType

foreign import ccall "MagickNegateImageChannel" magickNegateImageChannel
  :: Ptr MagickWand
  -> ChannelType
  -> MagickBooleanType       -- ^ negate gray
  -> IO MagickBooleanType

-- | MagickGetImageClipMask() gets the image clip mask at the current image index.
foreign import ccall "MagickGetImageClipMask" magickGetImageClipMask
  :: Ptr MagickWand
  -> IO (Ptr MagickWand)

-- | MagickSetImageClipMask() sets image clip mask.
foreign import ccall "MagickSetImageClipMask" magickSetImageClipMask
  :: Ptr MagickWand
  -> Ptr MagickWand
  -> IO MagickBooleanType

-- | MagickCompositeImage() composite one image onto another at the specified offset.
foreign import ccall "MagickCompositeImage" magickCompositeImage
  :: Ptr MagickWand
  -> Ptr MagickWand      -- ^ source
  -> CompositeOperator
  -> CSize               -- ^ column offset
  -> CSize               -- ^ row offset
  -> IO MagickBooleanType

foreign import ccall "MagickCompositeImage" magickCompositeImageChannel
  :: Ptr MagickWand
  -> Ptr MagickWand      -- ^ source
  -> ChannelType
  -> CompositeOperator
  -> CSize               -- ^ column offset
  -> CSize               -- ^ row offset
  -> IO MagickBooleanType

-- | MagickTransparentPaintImage() changes any pixel that matches color with the color defined by fill.
foreign import ccall "MagickTransparentPaintImage" magickTransparentPaintImage
  :: Ptr MagickWand
  -> Ptr PixelWand        -- ^ change this color to specified opacity value withing the image
  -> Double               -- ^ the level of transarency: 1.0 fully opaque 0.0 fully transparent
  -> Double               -- ^ By default target must match a particular pixel color exactly.
                          -- However, in many cases two colors may differ by a small amount.
                          -- The fuzz member of image defines how much tolerance is acceptable
                          -- to consider two colors as the same. For example, set fuzz to 10 and
                          -- the color red at intensities of 100 and 102 respectively are now
                          -- interpreted as the same color for the purposes of the floodfill.
  -> MagickBooleanType    -- paint any pixel that does not match the target color.
  -> IO MagickBooleanType

-- | MagickBorderImage() surrounds the image with a border of the color
-- defined by the bordercolor pixel wand.
foreign import ccall "MagickBorderImage" magickBorderImage
  :: Ptr MagickWand    -- ^ wand
  -> Ptr PixelWand     -- ^ bordercolor
  -> CSize             -- ^ width
  -> CSize             -- ^ height
  -> IO MagickBooleanType

-- | MagickShaveImage() shaves pixels from the image edges. It allocates
-- the memory necessary for the new Image structure and returns a pointer
-- to the new image.
foreign import ccall "MagickShaveImage" magickShaveImage
  :: Ptr MagickWand    -- ^ wand
  -> CSize             -- ^ columns
  -> CSize             -- ^ rows
  -> IO MagickBooleanType

-- | MagickSetImageAlphaChannel() activates, deactivates, resets, or
-- sets the alpha channel.
foreign import ccall "MagickSetImageAlphaChannel" magickSetImageAlphaChannel
  :: Ptr MagickWand    -- ^ wand
  -> AlphaChannelType  -- ^ alpha_type
  -> IO MagickBooleanType

-- | MagickNewImage() adds a blank image canvas of the specified size and background color to the wand.
foreign import ccall "MagickNewImage" magickNewImage
  :: Ptr MagickWand
  -> CSize                -- ^ width
  -> CSize                -- ^ height
  -> Ptr PixelWand        -- ^ background color
  -> IO MagickBooleanType

-- |  MagickDrawImage() renders the drawing wand on the current image.
foreign import ccall "MagickDrawImage" magickDrawImage
    :: Ptr MagickWand -> Ptr DrawingWand -> IO MagickBooleanType

-- | MagickFlopImage() creates a horizontal mirror image by reflecting the pixels around the central y-axis.
foreign import ccall "MagickFlopImage" magickFlopImage
  :: Ptr MagickWand -> IO MagickBooleanType


-- |  MagickAddNoiseImage() adds random noise to the image.
--    The type of noise: Uniform, Gaussian, Multiplicative, Impulse, Laplacian, or Poisson.
foreign import ccall "MagickAddNoiseImage" magickAddNoiseImage
  :: Ptr MagickWand -> NoiseType -> IO MagickBooleanType

-- | MagickAddImage() adds a clone of the images from the second wand and inserts them into the first wand.
-- Use MagickSetLastIterator(), to append new images into an existing wand, current image will be set to
-- last image so later adds with also be appened to end of wand.
--
-- Use MagickSetFirstIterator() to prepend new images into wand, any more images added will also be prepended
-- before other images in the wand. However the order of a list of new images will not change.
--
-- Otherwise the new images will be inserted just after the current image, and any later image will also be
-- added after this current image but before the previously added images. Caution is advised when multiple
-- image adds are inserted into the middle of the wand image list.
foreign import ccall "MagickAddImage" magickAddImage
  :: Ptr MagickWand -> Ptr MagickWand -> IO MagickBooleanType

-- | MagickFlipImage() creates a vertical mirror image by reflecting the pixels around the central x-axis.
foreign import ccall "MagickFlipImage" magickFlipImage
  :: Ptr MagickWand -> IO MagickBooleanType

-- | MagickSetImageVirtualPixelMethod() sets the image virtual pixel method.
--   the image virtual pixel method : UndefinedVirtualPixelMethod, ConstantVirtualPixelMethod,
--   EdgeVirtualPixelMethod, MirrorVirtualPixelMethod, or TileVirtualPixelMethod.
foreign import ccall "MagickSetImageVirtualPixelMethod" magickSetVirtualPixelMethod
  :: Ptr MagickWand -> VirtualPixelMethod -> IO VirtualPixelMethod

-- | MagickAppendImages() append the images in a wand from the current image onwards,
-- creating a new wand with the single image result. This is affected by the gravity
-- and background settings of the first image.
-- Typically you would call either MagickResetIterator() or MagickSetFirstImage() before
-- calling this function to ensure that all the images in the wand's image list will be appended together.
foreign import ccall "MagickAppendImages" magickAppendImages
  :: Ptr MagickWand -> MagickBooleanType -> IO (Ptr MagickWand)

-- | MagickWriteImage() writes an image to the specified filename. If the filename
-- parameter is NULL, the image is written to the filename set by MagickReadImage()
-- or MagickSetImageFilename().
foreign import ccall "MagickWriteImage" magickWriteImage
  :: Ptr MagickWand -> CString -> IO (MagickBooleanType)

-- | MagickBlurImage() blurs an image. We convolve the image with a gaussian
-- operator of the given radius and standard deviation (sigma). For reasonable
-- results, the radius should be larger than sigma. Use a radius of 0 and
-- BlurImage() selects a suitable radius for you.
--
-- The format of the MagickBlurImage method is:
foreign import ccall "MagickBlurImage" magickBlurImage
  :: Ptr MagickWand -> CDouble -> CDouble -> IO MagickBooleanType

foreign import ccall "MagickBlurImageChannel" magickBlurImageChannel
  :: Ptr MagickWand -> ChannelType -> CDouble -> CDouble -> IO MagickBooleanType

-- | MagickNormalizeImage() enhances the contrast of a color image by adjusting
--   the pixels color to span the entire range of colors available
--
--   You can also reduce the influence of a particular channel with a gamma
--   value of 0.
foreign import ccall "MagickNormalizeImage" magickNormalizeImage
  :: Ptr MagickWand -> IO MagickBooleanType

foreign import ccall "MagickNormalizeImageChannel" magickNormalizeImageChannel
  :: Ptr MagickWand -> ChannelType -> IO MagickBooleanType

-- | MagickShadowImage() simulates an image shadow.
foreign import ccall "MagickShadowImage" magickShadowImage
  :: Ptr MagickWand
  -> CDouble       -- ^ percentage transparency
  -> CDouble       -- ^ the standard deviation of the Gaussian, in pixels
  -> CSize         -- ^ the shadow x-offset
  -> CSize         -- ^ the shadow y-offset
  -> IO MagickBooleanType

-- | MagickTrimImage() remove edges that are the background color from the image.
foreign import ccall "MagickTrimImage" magickTrimImage
  :: Ptr MagickWand -> CDouble -> IO MagickBooleanType

-- | MagickResetImagePage() resets the Wand page canvas and position.
foreign import ccall "MagickResetImagePage" magickResetImagePage
  :: Ptr MagickWand -> CString -> IO MagickBooleanType

-- | MagickDistortImage() distorts an image using various distortion methods,
-- by mapping color lookups of the source image to a new destination image usally
-- of the same size as the source image, unless 'bestfit' is set to true.
-- If 'bestfit' is enabled, and distortion allows it, the destination image
-- is adjusted to ensure the whole source 'image' will just fit within the final
-- destination image, which will be sized and offset accordingly. Also in many cases
-- the virtual offset of the source image will be taken into account in the mapping.
foreign import ccall "MagickDistortImage" magickDistortImage
  :: Ptr MagickWand
  -> DistortImageMethod -- ^ the method of image distortion
  -> CSize              -- ^ the number of arguments given for this distortion method
  -> Ptr CDouble        -- ^ the arguments for this distortion method
  -> MagickBooleanType  -- ^ attempt to resize destination to fit distorted source
  -> IO MagickBooleanType

-- | MagickShadeImage() shines a distant light on an image to create
-- a three-dimensional effect. You control the positioning of the light
-- with azimuth and elevation; azimuth is measured in degrees off the x axis
-- and elevation is measured in pixels above the Z axis.
foreign import ccall "MagickShadeImage" magickShadeImage
  :: Ptr MagickWand
  -> MagickBooleanType -- ^ a value other than zero shades the intensity of each pixel
  -> CDouble           -- ^ azimuth of the light source direction
  -> CDouble           -- ^ evelation of the light source direction
  -> IO MagickBooleanType

-- | MagickColorizeImage() blends the fill color with each pixel in the image.
foreign import ccall "MagickColorizeImage" magickColorizeImage
  :: Ptr MagickWand
  -> Ptr PixelWand     -- ^ the colorize pixel wand
  -> Ptr PixelWand     -- ^ the opacity pixel wand
  -> IO MagickBooleanType

-- | MagickFxImage() evaluate expression for each pixel in the image.
foreign import ccall "MagickFxImage" magickFxImage
  :: Ptr MagickWand
  -> CString     -- ^ the expression
  -> IO (Ptr MagickWand)

-- | MagickFxImageChannel() evaluate expression for each pixel in the image.
foreign import ccall "MagickFxImageChannel" magickFxImageChannel
  :: Ptr MagickWand
  -> ChannelType -- ^ the image channel(s)
  -> CString     -- ^ the expression
  -> IO (Ptr MagickWand)

-- | MagickSigmoidalContrastImage() adjusts the contrast of an image with a
-- non-linear sigmoidal contrast algorithm. Increase the contrast of the image
-- using a sigmoidal transfer function without saturating highlights or shadows.
-- Contrast indicates how much to increase the contrast (0 is none; 3 is typical;
-- 20 is pushing it); mid-point indicates where midtones fall in the resultant
-- image (0 is white; 50 is middle-gray; 100 is black). Set sharpen to `True`
-- to increase the image contrast otherwise the contrast is reduced.
foreign import ccall "MagickSigmoidalContrastImage" magickSigmoidalContrastImage
  :: Ptr MagickWand
  -> MagickBooleanType -- ^ increase or decrease image contrast
  -> CDouble           -- ^ strength of the contrast, the larger the number the more 'threshold-like' it becomes
  -> CDouble           -- ^ midpoint of the function as a color value 0 to `quantumRange`
  -> IO MagickBooleanType

-- | see `magickSigmoidalContrastImage`
foreign import ccall "MagickSigmoidalContrastImageChannel" magickSigmoidalContrastImageChannel
  :: Ptr MagickWand
  -> ChannelType       -- ^ identify which channel to level: `redChannel`, `greenChannel`
  -> MagickBooleanType -- ^ increase or decrease image contrast
  -> CDouble           -- ^ strength of the contrast, the larger the number the more 'threshold-like' it becomes
  -> CDouble           -- ^ midpoint of the function as a color value 0 to `quantumRange`
  -> IO MagickBooleanType

-- | MagickEvaluateImage() applies an arithmetic, relational, or logical
-- expression to an image. Use these operators to lighten or darken an image,
-- to increase or decrease contrast in an image, or to produce the "negative"
-- of an image.
foreign import ccall "MagickEvaluateImage" magickEvaluateImage
  :: Ptr MagickWand
  -> MagickEvaluateOperator -- ^ a channel operator
  -> CDouble                -- ^ value
  -> IO MagickBooleanType

-- | see `magickEvaluateImage`
foreign import ccall "MagickEvaluateImages" magickEvaluateImages
  :: Ptr MagickWand
  -> MagickEvaluateOperator -- ^ a channel operator
  -> IO MagickBooleanType

-- | see `magickEvaluateImage`
foreign import ccall "MagickEvaluateImageChannel" magickEvaluateImageChannel
  :: Ptr MagickWand
  -> ChannelType            -- ^ the channel(s)
  -> MagickEvaluateOperator -- ^ a channel operator
  -> CDouble                -- ^ value
  -> IO MagickBooleanType

-- | MagickRollImage() offsets an image as defined by x and y.
foreign import ccall "MagickRollImage" magickRollImage
  :: Ptr MagickWand
  -> CDouble                -- ^ the x offset
  -> CDouble                -- ^ the y offset
  -> IO MagickBooleanType

-- | MagickAnnotateImage() annotates an image with text.
foreign import ccall "MagickAnnotateImage" magickAnnotateImage
  :: Ptr MagickWand
  -> Ptr DrawingWand -- ^ the draw wand
  -> CDouble         -- ^ x ordinate to left of text
  -> CDouble         -- ^ y ordinate to text baseline
  -> CDouble         -- ^ rotate text relative to this angle
  -> CString         -- ^ text to draw
  -> IO MagickBooleanType

-- | MagickMergeImageLayers() composes all the image layers from the current
-- given image onward to produce a single image of the merged layers.
-- The inital canvas's size depends on the given ImageLayerMethod, and is
-- initialized using the first images background color. The images are then
-- compositied onto that image in sequence using the given composition that
-- has been assigned to each individual image.
foreign import ccall "MagickMergeImageLayers" magickMergeImageLayers
  :: Ptr MagickWand
  -> ImageLayerMethod -- ^ the method of selecting the size of the initial canvas
  -> IO (Ptr MagickWand)

-- | MagickTintImage() applies a color vector to each pixel in the image. The 
-- length of the vector is 0 for black and white and at its maximum for the 
-- midtones. The vector weighting function is f(x)=(1-(4.0*((x-0.5)*(x-0.5)))).
foreign import ccall "MagickTintImage" magickTintImage 
  :: Ptr MagickWand
  -> Ptr PixelWand    -- ^ the tint pixel wand.
  -> Ptr PixelWand    -- ^ opacity pixel wand
  -> IO MagickBooleanType


-- | MagickSetImageMatte() sets the image matte channel.
foreign import ccall "MagickSetImageMatte" magickSetImageMatte
  :: Ptr MagickWand
  -> MagickBooleanType
  -> IO MagickBooleanType


