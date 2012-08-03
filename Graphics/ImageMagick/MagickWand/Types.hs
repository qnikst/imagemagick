module Graphics.ImageMagick.MagickWand.Types
  ( PPixelIterator
  , PPixelWand
  , PPixelPacket
  , PMagickWand
  ) where

import Foreign
import Control.Monad.Trans.Resource
import Graphics.ImageMagick.MagickWand.FFI.Types

type PPixelIterator = Ptr PixelIterator
type PPixelWand     = Ptr PixelWand
type PMagickWand    = Ptr MagickWand
type PPixelPacket   = ForeignPtr MagickPixelPacket


