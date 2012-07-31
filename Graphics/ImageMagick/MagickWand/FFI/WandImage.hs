{-# LINE 1 "Graphics/ImageMagick/MagickWand/FFI/WandImage.hsc" #-}
module Graphics.ImageMagick.MagickWand.FFI.WandImage
{-# LINE 2 "Graphics/ImageMagick/MagickWand/FFI/WandImage.hsc" #-}
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Graphics.ImageMagick.MagickWand.FFI.Types 


{-# LINE 11 "Graphics/ImageMagick/MagickWand/FFI/WandImage.hsc" #-}

-- | MagickGetImageHeight() returns the image height. 
foreign import ccall "MagickGetImageHeight" magickGetImageHeight
  :: Ptr MagickWand -> IO (CSize)

