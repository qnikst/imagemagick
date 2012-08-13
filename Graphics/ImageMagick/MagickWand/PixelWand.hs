module Graphics.ImageMagick.MagickWand.PixelWand
  ( pixelWand
  , setColor
  , setRedQuantum
  , getRedQuantum
  , setBlueQuantum
  , getBlueQuantum
  , setGreenQuantum
  , getGreenQuantum
  ) where

import           Control.Monad                                 (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                               (ByteString, useAsCString)

import           Graphics.ImageMagick.MagickWand.FFI.PixelWand as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils

pixelWand :: (MonadResource m) => m PPixelWand
pixelWand = fmap snd (allocate F.newPixelWand destroy)
  where destroy = void . F.destroyPixelWand

setColor :: (MonadResource m) => PPixelWand -> ByteString -> m ()
setColor p s = withException_ p $ useAsCString s (F.pixelSetColor p)

setRedQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setRedQuantum = (liftIO .) . F.pixelSetRedQuantum

getRedQuantum :: (MonadResource m) => PPixelWand -> m Quantum 
getRedQuantum =  liftIO . F.pixelGetRedQuantum

setGreenQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setGreenQuantum = (liftIO .) . F.pixelSetGreenQuantum

getGreenQuantum :: (MonadResource m) => PPixelWand -> m Quantum 
getGreenQuantum =  liftIO . F.pixelGetGreenQuantum

setBlueQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setBlueQuantum = (liftIO .) . F.pixelSetBlueQuantum

getBlueQuantum :: (MonadResource m) => PPixelWand -> m Quantum 
getBlueQuantum =  liftIO . F.pixelGetBlueQuantum
