{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.FFI.CacheView
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype VirtualPixelMethod = VirtualPixelMethod { unVirtualPixelMethod :: CInt }
          deriving (Eq, Show)

#{enum VirtualPixelMethod, VirtualPixelMethod,
    undefinedVirtualPixelMethod = UndefinedVirtualPixelMethod,
    backgroundVirtualPixelMethod = BackgroundVirtualPixelMethod,
    constantVirtualPixelMethod = ConstantVirtualPixelMethod,
    ditherVirtualPixelMethod = DitherVirtualPixelMethod,
    edgeVirtualPixelMethod = EdgeVirtualPixelMethod,
    mirrorVirtualPixelMethod = MirrorVirtualPixelMethod,
    randomVirtualPixelMethod = RandomVirtualPixelMethod,
    tileVirtualPixelMethod = TileVirtualPixelMethod,
    transparentVirtualPixelMethod = TransparentVirtualPixelMethod,
    maskVirtualPixelMethod = MaskVirtualPixelMethod,
    blackVirtualPixelMethod = BlackVirtualPixelMethod,
    grayVirtualPixelMethod = GrayVirtualPixelMethod,
    whiteVirtualPixelMethod = WhiteVirtualPixelMethod,
    horizontalTileVirtualPixelMethod = HorizontalTileVirtualPixelMethod,
    verticalTileVirtualPixelMethod = VerticalTileVirtualPixelMethod,
    horizontalTileEdgeVirtualPixelMethod = HorizontalTileEdgeVirtualPixelMethod,
    verticalTileEdgeVirtualPixelMethod = VerticalTileEdgeVirtualPixelMethod,
    checkerTileVirtualPixelMethod = CheckerTileVirtualPixelMethod
}
