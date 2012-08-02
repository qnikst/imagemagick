{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickWand.FFI.WandImage
  where

import Foreign
import Foreign.C.Types

import Graphics.ImageMagick.MagickWand.FFI.Types 

#include <wand/MagickWand.h>

-- | MagickGetImageHeight() returns the image height. 
foreign import ccall "MagickGetImageHeight" magickGetImageHeight
  :: Ptr MagickWand -> IO (CSize)

-- | MagickGetImageWidth() returns the image width.
foreign import ccall "MagickGetImageWidth" magickGetImageWidth
  :: Ptr MagickWand -> IO (CSize)

-- |  MagickGetImageCompressionQuality() gets the image compression quality.

foreign import ccall "MagickGetImageCompressionQuality" magickGetImageCompressionQuality
  :: Ptr MagickWand -> IO (CSize)

-- | MagickSetImageCompressionQuality() sets the image compression quality.
foreign import ccall "MagickSetImageCompressionQuality" magickSetImageCompressionQuality
  :: Ptr MagickWand -> CSize -> IO (MagickBooleanType)

