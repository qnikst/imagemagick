{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickWand.FFI.WandImage
  where

import           Foreign
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickCore.FFI.Composite
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
