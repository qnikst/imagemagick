{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Graphics.ImageMagick.MagickWand.Types
  ( PPixelIterator
  , PPixelWand
  , PPixelPacket
  , PMagickPixelPacket
  , PDrawingWand
  , PMagickWand
  , MagickWandException(..)
  -- * support for ImageMagick Exceptions
  , ExceptionCarrier(..)
  , module Graphics.ImageMagick.MagickCore.Types
  , Pixel(..)
  ) where

import qualified Data.Vector.Storable                              as V
import           Foreign
import           Foreign.C.String
import           Graphics.ImageMagick.MagickCore.Exception
import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.DrawingWand   as F
import           Graphics.ImageMagick.MagickWand.FFI.MagickWand    as F
import           Graphics.ImageMagick.MagickWand.FFI.PixelIterator as F
import           Graphics.ImageMagick.MagickWand.FFI.PixelWand     as F
import           Graphics.ImageMagick.MagickWand.FFI.Types

type PPixelIterator     = Ptr PixelIterator
type PPixelWand         = Ptr PixelWand
type PMagickWand        = Ptr MagickWand
type PDrawingWand       = Ptr DrawingWand
type PMagickPixelPacket = ForeignPtr MagickPixelPacket
type PPixelPacket       = ForeignPtr PixelPacket

constructException :: forall t.
  (t -> Ptr ExceptionType -> IO CString)
  -> t -> IO MagickWandException
constructException f w = alloca $ \x -> do
    s  <- peekCString =<< f w x
    x' <- peek x
    return $ MagickWandException (toSeverity x') x' s
{-# INLINE constructException #-}

instance ExceptionCarrier (Ptr MagickWand) where
  getException = constructException F.magickGetException

instance ExceptionCarrier (Ptr PixelIterator) where
  getException = constructException F.pixelGetIteratorException

instance ExceptionCarrier (Ptr PixelWand) where
  getException = constructException F.pixelGetException

instance ExceptionCarrier (Ptr DrawingWand) where
  getException = constructException F.drawGetException

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
