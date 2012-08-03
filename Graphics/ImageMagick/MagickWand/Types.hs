module Graphics.ImageMagick.MagickWand.Types
  ( PPixelIterator
  , PPixelWand
  , PPixelPacket
  , PMagickWand
  ) where

import           Control.Monad.Trans.Resource
import           Foreign
import           Graphics.ImageMagick.MagickWand.FFI.Types

type PPixelIterator = Ptr PixelIterator
type PPixelWand     = Ptr PixelWand
type PMagickWand    = Ptr MagickWand
type PPixelPacket   = ForeignPtr MagickPixelPacket


