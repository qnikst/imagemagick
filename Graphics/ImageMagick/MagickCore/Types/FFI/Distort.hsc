{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickCore.Types.FFI.Distort
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype DistortImageMethod = DistortImageMethod { unDistortImageMethod :: CInt }

#{enum DistortImageMethod, DistortImageMethod
  , undefinedDistortion = UndefinedDistortion
  , affineDistortion = AffineDistortion
  , affineProjectionDistortion = AffineProjectionDistortion
  , scaleRotateTranslateDistortion = ScaleRotateTranslateDistortion
  , perspectiveDistortion = PerspectiveDistortion
  , perspectiveProjectionDistortion = PerspectiveProjectionDistortion
  , bilinearForwardDistortion = BilinearForwardDistortion
  , bilinearReverseDistortion = BilinearReverseDistortion
  , polynomialDistortion = PolynomialDistortion
  , arcDistortion = ArcDistortion
  , polarDistortion = PolarDistortion
  , dePolarDistortion = DePolarDistortion
  , cylinder2PlaneDistortion = Cylinder2PlaneDistortion
  , plane2CylinderDistortion = Plane2CylinderDistortion
  , barrelDistortion = BarrelDistortion
  , barrelInverseDistortion = BarrelInverseDistortion
  , shepardsDistortion = ShepardsDistortion
  , resizeDistortion = ResizeDistortion
  , sentinelDistortion = SentinelDistortion
}

bilinearDistortion = bilinearForwardDistortion