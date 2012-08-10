{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickCore.Types.FFI.PaintMethod
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype PaintMethod =  PaintMethod { unPaintMethod :: CInt }


#{enum PaintMethod, PaintMethod,
  undefinedMethod = UndefinedMethod,
  pointMethod = PointMethod,
  replaceMethod = ReplaceMethod,
  floodfillMethod = FloodfillMethod,
  fillToBorderMethod = FillToBorderMethod,
  resetMethod = ResetMethod
}

