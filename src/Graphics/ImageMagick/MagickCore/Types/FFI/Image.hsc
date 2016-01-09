{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickCore.Types.FFI.Image
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>
newtype ImageType = ImageType { unImageType :: CInt }
  deriving (Eq, Show)

#{enum ImageType, ImageType
  , undefinedType = UndefinedType
  , bilevelType = BilevelType
  , grayscaleType = GrayscaleType
  , grayscaleMatteType = GrayscaleMatteType
  , paletteType = PaletteType
  , paletteMatteType = PaletteMatteType
  , trueColorType = TrueColorType
  , trueColorMatteType = TrueColorMatteType
  , colorSeparationType = ColorSeparationType
  , colorSeparationMatteType = ColorSeparationMatteType
  , optimizeType = OptimizeType
  , paletteBilevelMatteType = PaletteBilevelMatteType
}
