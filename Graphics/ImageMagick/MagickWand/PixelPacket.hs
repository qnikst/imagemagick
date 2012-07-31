{-# LANGUAGE PackageImports #-}
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

import Foreign
import Control.Monad.IO.Class
import qualified Graphics.ImageMagick.MagickWand.FFI.Types as F
import Graphics.ImageMagick.MagickWand.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.PixelIterator as F
import qualified Graphics.ImageMagick.MagickWand.FFI.PixelWand     as F


getPixel' :: (MonadIO m) => (Ptr F.MagickPixelPacket -> IO F.MagickRealType) -> PPixelPacket ->  m F.MagickRealType
getPixel' f wp = liftIO $ withForeignPtr wp f
{-# INLINE getPixel' #-}

getPixelRed :: (MonadIO m) => PPixelPacket -> m F.MagickRealType
getPixelRed  = getPixel' F.getPixelRed

getPixelBlue :: (MonadIO m) => PPixelPacket -> m F.MagickRealType
getPixelBlue  = getPixel' F.getPixelBlue

getPixelGreen :: (MonadIO m) => PPixelPacket -> m F.MagickRealType
getPixelGreen  = getPixel' F.getPixelGreen

getPixelIndex :: (MonadIO m) => PPixelPacket -> m F.MagickRealType
getPixelIndex  = getPixel' F.getPixelIndex

setPixel' :: (MonadIO m) => (Ptr F.MagickPixelPacket -> F.MagickRealType -> IO ()) -> PPixelPacket -> F.MagickRealType -> m ()
setPixel' f wp c = liftIO $ withForeignPtr wp (flip f c)
{-# INLINE setPixel' #-}

setPixelRed :: (MonadIO m) => PPixelPacket -> F.MagickRealType -> m ()
setPixelRed = setPixel' F.setPixelRed 

setPixelIndex :: (MonadIO m) => PPixelPacket -> F.MagickRealType -> m ()
setPixelIndex = setPixel' F.setPixelIndex 

setPixelGreen :: (MonadIO m) => PPixelPacket -> F.MagickRealType -> m ()
setPixelGreen = setPixel' F.setPixelGreen 

setPixelBlue :: (MonadIO m) => PPixelPacket -> F.MagickRealType -> m ()
setPixelBlue = setPixel' F.setPixelBlue 
