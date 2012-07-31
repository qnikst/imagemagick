{-# LANGUAGE CPP, ForeignFunctionInterface, NoMonomorphismRestriction #-}
module Graphics.ImageMagick.MagickWand.FFI.WandImage
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Graphics.ImageMagick.MagickWand.FFI.Types 

#include <wand/MagickWand.h>

-- | MagickGetImageHeight() returns the image height. 
foreign import ccall "MagickGetImageHeight" magickGetImageHeight
  :: Ptr MagickWand -> IO (CSize)

