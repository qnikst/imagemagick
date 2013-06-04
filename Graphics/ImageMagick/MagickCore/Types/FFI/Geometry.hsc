{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.ImageMagick.MagickCore.Types.FFI.Geometry
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype GravityType = GravityType { unGravityType :: CInt }
          deriving (Eq, Show)

#{enum GravityType, GravityType
  , forgetGravity = ForgetGravity
  , northWestGravity = NorthWestGravity
  , northGravity = NorthGravity
  , northEastGravity = NorthEastGravity
  , westGravity = WestGravity 
  , centerGravity = CenterGravity
  , eastGravity = EastGravity
  , southWestGravity = SouthWestGravity
  , southGravity = SouthGravity
  , southEastGravity = SouthEastGravity
  , staticGravity = StaticGravity
}

undefinedGravity :: GravityType
undefinedGravity = forgetGravity

