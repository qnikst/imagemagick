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
  -> IO ()

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

