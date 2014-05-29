{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.ImageMagick.MagickCore.Types.FFI.DitherMethod
    where

import           Foreign.C.Types
import           Foreign.Storable
#include <magick/MagickCore.h>

newtype DitherMethod = DitherMethod { unDitherMethod :: CInt }
    deriving (Eq,Show,Storable)

#{enum DitherMethod, DitherMethod
  , undefinedDitherFilter      = UndefinedDitherMethod
  , noDitherMethod             = NoDitherMethod
  , riemersmaDitherMethod      = RiemersmaDitherMethod
  , floydSteinbergDitherMethod = FloydSteinbergDitherMethod
}

