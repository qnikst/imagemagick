{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Graphics.ImageMagick.MagickWand.Types
  ( PPixelIterator
  , PPixelWand
  , PPixelPacket
  , PDrawingWand
  , PMagickWand
  , MagickRealType
  , ImageWandException(..)
  -- * support for ImageMagick Exceptions
  , ExceptionCarrier(..)
  , module Graphics.ImageMagick.MagickCore.Types
  , Pixel(..)
  ) where

import           Control.Exception.Base
import           Control.Monad.Trans.Resource                      ()
import           Data.Typeable
import qualified Data.Vector.Storable                              as V
import           Foreign
import           Foreign.C.String
import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.DrawingWand   as F
import           Graphics.ImageMagick.MagickWand.FFI.MagickWand    as F
import           Graphics.ImageMagick.MagickWand.FFI.PixelIterator as F
import           Graphics.ImageMagick.MagickWand.FFI.PixelWand     as F
import           Graphics.ImageMagick.MagickWand.FFI.Types

type PPixelIterator = Ptr PixelIterator
type PPixelWand     = Ptr PixelWand
type PMagickWand    = Ptr MagickWand
type PDrawingWand   = Ptr DrawingWand
type PPixelPacket   = ForeignPtr MagickPixelPacket

data ImageWandException = ImageWandException ExceptionType String
  deriving (Typeable)

instance Show (ImageWandException) where
  show (ImageWandException _ s) = s

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

instance ExceptionCarrier (Ptr PixelIterator) where
  getException w = alloca $ \x -> do
       s <- peekCString =<< F.pixelGetIteratorException w x
       x' <- peek x
       return $ ImageWandException x' s

instance ExceptionCarrier (Ptr PixelWand) where
  getException w = alloca $ \x -> do
      s <- peekCString =<< F.pixelGetException w x
      x' <- peek x
      return $ ImageWandException x' s

instance ExceptionCarrier (Ptr DrawingWand) where
  getException w = alloca $ \x -> do
      s <- peekCString =<< F.drawGetException w x
      x' <- peek x
      return $ ImageWandException x' s

class (Storable a) => Pixel a where
  pixelStorageType :: [a] -> StorageType
  withPixels :: [a] -> (Ptr a -> IO b) -> IO b
  withPixels xs f = V.unsafeWith (V.fromList xs) f

instance Pixel Word8 where
  pixelStorageType = const charPixel

instance Pixel Word16 where
  pixelStorageType = const shortPixel

instance Pixel Word32 where
  pixelStorageType = const longPixel

instance Pixel Word64 where
  pixelStorageType = const longPixel

instance Pixel Float where
  pixelStorageType = const floatPixel

instance Pixel Double where
  pixelStorageType = const doublePixel
