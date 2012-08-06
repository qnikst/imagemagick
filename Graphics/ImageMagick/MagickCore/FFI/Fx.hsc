{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.ImageMagick.MagickCore.FFI.Fx
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype NoiseType = NoiseType { unNoiseType :: CInt }
          deriving (Eq, Show)

#{enum NoiseType, NoiseType,
  undefinedNoise = UndefinedNoise,
  uniformNoise = UniformNoise,
  gaussianNoise = GaussianNoise,
  multiplicativeGaussianNoise = MultiplicativeGaussianNoise,
  impulseNoise = ImpulseNoise,
  laplacianNoise = LaplacianNoise,
  poissonNoise = PoissonNoise,
  randomNoise = RandomNoise
}
