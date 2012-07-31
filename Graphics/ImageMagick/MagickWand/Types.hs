module Graphics.ImageMagick.MagickWand.Types
  ( PPixelIterator
  , PPixelWand
  , PPixelPacket
  ) where

import Foreign
import Control.Monad.Trans.Resource
import Graphics.ImageMagick.MagickWand.FFI.Types

type PPixelIterator = Ptr PixelIterator
type PPixelWand     = Ptr PixelWand
type PPixelPacket   = ForeignPtr MagickPixelPacket


