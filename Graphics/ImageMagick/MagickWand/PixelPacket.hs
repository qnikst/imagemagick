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


getPixel' :: (MonadIO m) => (Ptr F.MagickPixelPacket -> IO MagickRealType) -> PPixelPacket ->  m MagickRealType
getPixel' f wp = liftIO $ withForeignPtr wp f
{-# INLINE getPixel' #-}

getPixelRed :: (MonadIO m) => PPixelPacket -> m MagickRealType
getPixelRed  = getPixel' F.getPixelRed

getPixelBlue :: (MonadIO m) => PPixelPacket -> m MagickRealType
getPixelBlue  = getPixel' F.getPixelBlue

getPixelGreen :: (MonadIO m) => PPixelPacket -> m MagickRealType
getPixelGreen  = getPixel' F.getPixelGreen

getPixelIndex :: (MonadIO m) => PPixelPacket -> m MagickRealType
getPixelIndex  = getPixel' F.getPixelIndex

setPixel' :: (MonadIO m) => (Ptr F.MagickPixelPacket -> MagickRealType -> IO ()) -> PPixelPacket -> MagickRealType -> m ()
setPixel' f wp c = liftIO $ withForeignPtr wp (`f` c)
{-# INLINE setPixel' #-}

setPixelRed :: (MonadIO m) => PPixelPacket -> MagickRealType -> m ()
setPixelRed = setPixel' F.setPixelRed

setPixelIndex :: (MonadIO m) => PPixelPacket -> MagickRealType -> m ()
setPixelIndex = setPixel' F.setPixelIndex

setPixelGreen :: (MonadIO m) => PPixelPacket -> MagickRealType -> m ()
setPixelGreen = setPixel' F.setPixelGreen

setPixelBlue :: (MonadIO m) => PPixelPacket -> MagickRealType -> m ()
setPixelBlue = setPixel' F.setPixelBlue
