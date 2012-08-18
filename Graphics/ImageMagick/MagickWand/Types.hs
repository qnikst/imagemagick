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
  , Pixels(..)
  ) where

import           Control.Exception.Base
import           Control.Monad.Trans.Resource                      ()
import           Data.Typeable
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
  data Pixels a :: *
  pixels :: Pixels a -> [a]
  pixelStorageType :: (Pixels a) -> StorageType
  allocaPixels :: Int -> (Ptr a -> IO b) -> IO b
  allocaPixels n f = allocaArray n (\(p :: Ptr a) -> f p)
  withTypedPixels :: Pixels a -> (StorageType -> Ptr a -> IO b) -> IO b
  withTypedPixels p f = withArray (pixels p) (f (pixelStorageType p))

instance Pixel Int8 where
  data Pixels Int8 = CharPixels [Int8]
  pixelStorageType = const charPixel

instance Pixel Int16 where
  data Pixels Int16 = ShortPixels [Int16]
  pixelStorageType = const shortPixel

instance Pixel Int32 where
  data Pixels Int32 = IntegerPixels [Int32]
  pixelStorageType = const longPixel

instance Pixel Int64 where
  data Pixels Int64 = LongPixels [Int64]
  pixelStorageType = const longPixel

instance Pixel Float where
  data Pixels Float = FloatPixels [Float]
  pixelStorageType = const floatPixel

instance Pixel Double where
  data Pixels Double = DoublePixels [Double]
  pixelStorageType = const doublePixel
  {-

data Pixels =   | ShortPixels [Int16] | IntegerPixels [Int32] |
              LongPixels [Int64] | FloatPixels [Float] | DoublePixels [Double]
            deriving (Eq, Show)
-}
