{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.Types.FFI.MagickFunction
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype MagickFunction = MagickFunction { unMagickFunction :: CInt }
          deriving (Eq, Show)

#{enum MagickFunction, MagickFunction,
  undefinedFunction =   UndefinedFunction,
  polynomialFunction =   PolynomialFunction,
  sinusoidFunction =   SinusoidFunction,
  arcsinFunction =   ArcsinFunction,
  arctanFunction = ArctanFunction
}

