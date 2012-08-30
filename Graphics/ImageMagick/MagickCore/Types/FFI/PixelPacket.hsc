{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 
module Graphics.ImageMagick.MagickCore.Types.FFI.PixelPacket
    where

import           Data.Int
import           Data.Word
import           Foreign
import           Foreign.C.Types
#include <magick/MagickCore.h>

data PixelPacket 

instance Storable PixelPacket where
  sizeOf = const #size PixelPacket
  alignment _ = 1

pixelPacketGetRed     = #peek PixelPacket, red
pixelPacketGetGreen   = #peek PixelPacket, green
pixelPacketGetBlue    = #peek PixelPacket, blue
pixelPacketGetOpacity = #peek PixelPacket, opacity
pixelPacketSetRed     = #poke PixelPacket, red
pixelPacketSetGreen   = #poke PixelPacket, green
pixelPacketSetBlue    = #poke PixelPacket, blue
pixelPacketSetOpacity = #poke PixelPacket, opacity

