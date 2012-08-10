{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.FFI.Log
    where

import           Foreign.C.String
import           Foreign.C.Types
import           Graphics.ImageMagick.MagickCore.Types.FFI.Log
#include <magick/MagickCore.h>


-- | SetLogEventMask() accepts a list that determines which events to log.  All
-- other events are ignored.  By default, no debug is enabled.  This method
-- returns the previous log event mask.
foreign import ccall "SetLogEventMask" setLogEventMask
  :: CString -> IO LogEventType
