{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.FFI.Mime
    where

import           Foreign.C.String
#include <magick/MagickCore.h>


-- | MagickToMime() returns the officially registered (or de facto) MIME
-- media-type corresponding to a magick string.  If there is no registered
-- media-type, then the string "image/x-magick" (all lower case) is returned.
-- The returned string must be deallocated by the user.
foreign import ccall "MagickToMime" magickToMime
  :: CString -> IO CString
