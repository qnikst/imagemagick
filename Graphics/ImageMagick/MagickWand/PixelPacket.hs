module Graphics.ImageMagick.MagickWand.PixelPacket
  ( getPixelRed
  , setPixelRed
  , getPixelBlue
  , setPixelBlue
  , getPixelGreen
  , setPixelGreen
  , getPixelIndex
  , setPixelIndex
  ) where

import           Control.Monad.IO.Class
import           Foreign
import qualified Graphics.ImageMagick.MagickWand.FFI.Types         as F
import           Graphics.ImageMagick.MagickWand.Types


getPixel' :: (MonadIO m) => (Ptr F.MagickPixelPacket -> IO MagickRealType) -> PMagickPixelPacket ->  m MagickRealType
getPixel' f wp = liftIO $ withForeignPtr wp f
{-# INLINE getPixel' #-}

getPixelRed :: (MonadIO m) => PMagickPixelPacket -> m MagickRealType
getPixelRed  = getPixel' F.getPixelRed

getPixelBlue :: (MonadIO m) => PMagickPixelPacket -> m MagickRealType
getPixelBlue  = getPixel' F.getPixelBlue

getPixelGreen :: (MonadIO m) => PMagickPixelPacket -> m MagickRealType
getPixelGreen  = getPixel' F.getPixelGreen

getPixelIndex :: (MonadIO m) => PMagickPixelPacket -> m MagickRealType
getPixelIndex  = getPixel' F.getPixelIndex

setPixel' :: (MonadIO m) => (Ptr F.MagickPixelPacket -> MagickRealType -> IO ()) -> PMagickPixelPacket -> MagickRealType -> m ()
setPixel' f wp c = liftIO $ withForeignPtr wp (`f` c)
{-# INLINE setPixel' #-}

setPixelRed :: (MonadIO m) => PMagickPixelPacket -> MagickRealType -> m ()
setPixelRed = setPixel' F.setPixelRed

setPixelIndex :: (MonadIO m) => PMagickPixelPacket -> MagickRealType -> m ()
setPixelIndex = setPixel' F.setPixelIndex

setPixelGreen :: (MonadIO m) => PMagickPixelPacket -> MagickRealType -> m ()
setPixelGreen = setPixel' F.setPixelGreen

setPixelBlue :: (MonadIO m) => PMagickPixelPacket -> MagickRealType -> m ()
setPixelBlue = setPixel' F.setPixelBlue
