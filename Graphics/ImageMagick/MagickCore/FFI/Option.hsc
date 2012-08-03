{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.FFI.Option
  where

import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickWand.FFI.Types

#include <magick/MagickCore.h>

-- | ParseChannelOption() parses channel type string representation

foreign import ccall "ParseChannelOption" parseChannelOption
  :: CString -> IO ChannelType

