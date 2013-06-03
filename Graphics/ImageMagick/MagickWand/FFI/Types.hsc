{-# LANGUAGE CPP, ForeignFunctionInterface, NoMonomorphismRestriction, RankNTypes #-}

module Graphics.ImageMagick.MagickWand.FFI.Types
  where

#include <wand/MagickWand.h>

import Control.Monad

import Foreign
import Foreign.C.Types

data PixelIterator
data MagickWand
data PixelWand
data DrawingWand
data Image
data PointInfo = PointInfo {piX, piY :: CDouble}
               deriving (Eq, Show)

instance Storable PointInfo where
  sizeOf = const #size PointInfo
  alignment _ = 1
  poke p foo  = do
    #{poke PointInfo, x} p $ piX foo
    #{poke PointInfo, y} p $ piY foo

  peek p = return PointInfo
              `ap` (#{peek PointInfo, x} p)
              `ap` (#{peek PointInfo, y} p)


newtype MagickBooleanType = MagickBooleanType { unMagickBooleanType :: CInt}
  deriving (Eq, Show)

#{enum MagickBooleanType, MagickBooleanType 
  , mFalse = MagickFalse
  , mTrue  = MagickTrue
}

newtype ClassType = ClassType { unClassType :: CInt}
  deriving (Eq, Show)

#{enum ClassType, ClassType
  , undefinedClass = UndefinedClass
  , directClass    = DirectClass
  , pseudoClass    = PseudoClass
}

newtype LineCap = LineCap { unLineCap :: CInt }
#{enum LineCap, LineCap
  , udefinedCap = UndefinedCap
  , buttCap = ButtCap
  , roundCap = RoundCap
  , squareCap = SquareCap
}

newtype LineJoin = LineJoin { unLineJoin :: CInt }
#{enum LineJoin, LineJoin
  , undefinedJoin = UndefinedJoin
  , mitterJoin = MiterJoin
  , roundJoin = RoundJoin
  , bevelJoin = BevelJoin
}

newtype FillRule = FillRule { unFillRule :: CInt }
#{enum FillRule, FillRule
 , undefinedRule = UndefinedRule
 , evenOddRule = EvenOddRule
 , nonZeroRule = NonZeroRule
}

data MagickPixelPacket 

instance Storable MagickPixelPacket where
  sizeOf = const #size MagickPixelPacket
  alignment _ = 1

getPixelRed, getPixelGreen, getPixelBlue, getPixelIndex
  :: Storable a => Ptr b -> IO a

getPixelRed   = #peek MagickPixelPacket, red
getPixelGreen = #peek MagickPixelPacket, green
getPixelBlue  = #peek MagickPixelPacket, blue
getPixelIndex = #peek MagickPixelPacket, index

setPixelRed, setPixelGreen, setPixelBlue, setPixelIndex
  :: Storable a => Ptr b -> a -> IO ()
  
setPixelRed   = #poke MagickPixelPacket, red
setPixelGreen = #poke MagickPixelPacket, green
setPixelBlue  = #poke MagickPixelPacket, blue
setPixelIndex = #poke MagickPixelPacket, index

