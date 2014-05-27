{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.FFI.Quantize
    where

#include <magick/MagickCore.h>

import Foreign
import Graphics.ImageMagick.MagickCore.Types.FFI.Quantize

foreign import ccall unsafe "GetQuantizeInfo"
  c_getQuantizeInfo :: Ptr QuantizeInfo -> IO ()

