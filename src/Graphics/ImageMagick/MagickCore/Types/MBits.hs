module Graphics.ImageMagick.MagickCore.Types.MBits
  where

import           Data.Bits
import           Graphics.ImageMagick.MagickCore.Types.FFI.ChannelType

class MBits a where
  (^|^) :: a -> a -> a
  (^&^) :: a -> a -> a

instance MBits ChannelType where
  a ^|^ b = ChannelType (unChannelType a .|. unChannelType b)
  a ^&^ b = ChannelType (unChannelType a .&. unChannelType b)
