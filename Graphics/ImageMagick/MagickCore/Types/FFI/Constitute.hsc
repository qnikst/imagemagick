{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.Types.FFI.Constitute
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype StorageType = StorageType { unStorageType :: CInt }
          deriving (Eq, Show)

#{enum StorageType, StorageType
  , undefinedPixel = UndefinedPixel
  , charPixel = CharPixel
  , doublePixel = DoublePixel
  , floatPixel = FloatPixel
  , integerPixel = IntegerPixel
  , longPixel = LongPixel
  , quantumPixel = QuantumPixel
  , shortPixel = ShortPixel
}
