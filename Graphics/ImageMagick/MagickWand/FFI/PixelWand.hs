{-# LINE 1 "Graphics/ImageMagick/MagickWand/FFI/PixelWand.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Graphics/ImageMagick/MagickWand/FFI/PixelWand.hsc" #-}

module Graphics.ImageMagick.MagickWand.FFI.PixelWand 
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Graphics.ImageMagick.MagickWand.FFI.Types 


{-# LINE 13 "Graphics/ImageMagick/MagickWand/FFI/PixelWand.hsc" #-}

-- | DestroyPixelWand() deallocates resources associated with a PixelWand.

foreign import ccall "DestroyPixelWand" destroyPixelWand
  :: Ptr PixelWand -> IO (Ptr PixelWand)


-- | PixelGetMagickColor() gets the magick color of the pixel wand.

foreign import ccall "PixelGetMagickColor" pixelGetMagickColor
  :: Ptr PixelWand -> Ptr (MagickPixelPacket) -> IO ()

-- | PixelSetMagickColor() sets the color of the pixel wand.

foreign import ccall "PixelSetMagickColor" pixelSetMagickColor
  :: Ptr PixelWand -> Ptr (MagickPixelPacket) -> IO ()

