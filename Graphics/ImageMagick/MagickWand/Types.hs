{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
module Graphics.ImageMagick.MagickWand.Types
  ( PPixelIterator
  , PPixelWand
  , PPixelPacket
  , PMagickWand
  , MagickRealType
  , ImageWandException(..)
  -- * support for ImageMagick Exceptions
  , ExceptionCarrier(..)
  , module Graphics.ImageMagick.MagickCore.Types
  ) where

import           Control.Exception.Base
import           Control.Monad.Trans.Resource
import           Data.Typeable
import           Foreign
import           Foreign.C.String
import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.MagickWand as F
import           Graphics.ImageMagick.MagickWand.FFI.Types

type PPixelIterator = Ptr PixelIterator
type PPixelWand     = Ptr PixelWand
type PMagickWand    = Ptr MagickWand
type PPixelPacket   = ForeignPtr MagickPixelPacket

data ImageWandException = ImageWandException ExceptionType String
  deriving (Typeable)

instance Show (ImageWandException) where
  show (ImageWandException e s) = s

instance Exception ImageWandException

-- * Exception Carrier can be different objects
-- that are used in functions

class ExceptionCarrier a where
  getException :: a -> IO ImageWandException

instance ExceptionCarrier (Ptr MagickWand) where
  getException w = alloca $ \x -> do
        s <- peekCString =<< F.magickGetException w x
        x' <- peek x
        return $ ImageWandException x' s

