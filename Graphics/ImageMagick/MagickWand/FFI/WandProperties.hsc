{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickWand.FFI.WandProperties
  where

import           Foreign
import           Foreign.C.String

import           Graphics.ImageMagick.MagickWand.FFI.Types


-- | MagickSetOption() associates one or options with the wand
-- (e.g. MagickSetOption(wand,"jpeg:perserve","yes")).
foreign import ccall "MagickSetOption" magickSetOption
  :: Ptr MagickWand
  -> CString        -- ^ the key
  -> CString        -- ^ the value
  -> IO MagickBooleanType


