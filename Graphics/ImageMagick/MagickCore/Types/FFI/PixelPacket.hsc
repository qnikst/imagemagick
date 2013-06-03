{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 
module Graphics.ImageMagick.MagickCore.Types.FFI.PixelPacket
    where

import           Foreign
#include <magick/MagickCore.h>

data PixelPacket 

instance Storable PixelPacket where
  sizeOf = const #size PixelPacket
  alignment _ = 1

pixelPacketGetRed, pixelPacketGetGreen, pixelPacketGetBlue
                 , pixelPacketGetOpacity 
  :: Storable a => Ptr b -> IO a
pixelPacketGetRed     = #peek PixelPacket, red
pixelPacketGetGreen   = #peek PixelPacket, green
pixelPacketGetBlue    = #peek PixelPacket, blue
pixelPacketGetOpacity = #peek PixelPacket, opacity

pixelPacketSetRed, pixelPacketSetGreen, pixelPacketSetBlue
                 , pixelPacketSetOpacity
  :: Storable a => Ptr b -> a -> IO ()

pixelPacketSetRed     = #poke PixelPacket, red
pixelPacketSetGreen   = #poke PixelPacket, green
pixelPacketSetBlue    = #poke PixelPacket, blue
pixelPacketSetOpacity = #poke PixelPacket, opacity

