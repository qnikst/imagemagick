{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.Types.FFI.FilterTypes
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype FilterTypes = FilterTypes { unPCREOption :: CInt }
    deriving (Eq,Show)

#{enum FilterTypes, FilterTypes
  , undefinedFilter = UndefinedFilter
  , pointFilter     = PointFilter
  , boxFilter       = BoxFilter
  , triangleFilter  = TriangleFilter
  , hermiteFilter   = HermiteFilter
  , hanningFilter   = HanningFilter
  , hammingFilter   = HammingFilter
  , blackmanFilter  = BlackmanFilter
  , gaussianFilter  = GaussianFilter
  , qaudraticFilter = QuadraticFilter
  , cubicFilter     = CubicFilter
  , catromFilter    = CatromFilter
  , mirchellFilter  = MitchellFilter
  , jincFilter      = JincFilter
  , sinkFilter      = SincFilter
  , sinkFastFilter  = SincFastFilter
  , kaiserFilter    = KaiserFilter
  , welshFilter     = WelshFilter
  , parzenFilter    = ParzenFilter
  , bohmanFilter    = BohmanFilter
  , bartlettFilter  = BartlettFilter
  , lagrangeFilter  = LagrangeFilter
  , lanczosFilter   = LanczosFilter
  , lanczosSharpFilter = LanczosSharpFilter
  , lanczos2Filter  = Lanczos2Filter
  , lanczos2SharpFilter = Lanczos2SharpFilter
  , robidouxFilter  = RobidouxFilter
}

