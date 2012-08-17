{-# LANGUAGE CPP #-}
module Graphics.ImageMagick.MagickWand.PixelWand
  ( pixelWand
  , setColor
  , setRedQuantum
  , getRed
  , getRedQuantum
  , setBlueQuantum
  , getBlue
  , getBlueQuantum
  , setGreenQuantum
  , getGreen
  , getGreenQuantum
-- , clearPixelWand 
-- , cloneWand 
-- , cloneWands 
-- , destroyWand 
-- , destroyWands 
-- , isWandSimilar 
-- , isWand 
-- , newWands 
-- , getAlpha 
-- , getAlphaQuantum 
-- , getBlack 
-- , getBlackQuantum 
-- , getBlue 
-- , getBlueQuantum 
-- , getColorAsString 
-- , getColorAsNormalizedString 
-- , getColorCount 
-- , getCyan 
-- , getCyanQuantum 
-- , getFuzz 
-- , getHSL 
-- , getIndex 
-- , getMagenta 
-- , getMagentaQuantum 
-- , getMagickColor 
-- , getOpacity 
-- , getOpacityQuantum 
-- , getQuantumColor 
-- , getRed 
-- , getRedQuantum 
-- , getYellow 
-- , getYellowQuantum 
-- , setAlpha 
-- , setAlphaQuantum 
-- , setBlack 
-- , setBlackQuantum 
-- , setBlue 
-- , setBlueQuantum 
-- , setColor 
-- , setColorCount 
-- , setColorFromWand 
-- , setCyan 
-- , setCyanQuantum 
-- , setFuzz 
-- , setGreen 
-- , setGreenQuantum 
-- , setHSL 
-- , setIndex 
-- , setMagenta 
-- , setMagentaQuantum 
-- , setMagickColor 
-- , setOpacity 
-- , setOpacityQuantum 
-- , setQuantumColor 
-- , setRed 
-- , setRedQuantum 
-- , setYellow 
-- , setYellowQuantum
  ) where

import           Control.Monad                                 (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                               (ByteString, useAsCString)

import           Graphics.ImageMagick.MagickWand.FFI.PixelWand as F
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils

pixelWand :: (MonadResource m) => m PPixelWand
pixelWand = fmap snd (allocate F.newPixelWand destroy)
  where destroy = void . F.destroyPixelWand

setColor :: (MonadResource m) => PPixelWand -> ByteString -> m ()
setColor p s = withException_ p $ useAsCString s (F.pixelSetColor p)

setRedQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setRedQuantum = (liftIO .) . F.pixelSetRedQuantum

getRed :: (MonadResource m) => PPixelWand -> m Double
getRed = (fmap realToFrac) . liftIO . F.pixelGetRed

getRedQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getRedQuantum =  liftIO . F.pixelGetRedQuantum

setGreenQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setGreenQuantum = (liftIO .) . F.pixelSetGreenQuantum

getGreen :: (MonadResource m) => PPixelWand -> m Double
getGreen = (fmap realToFrac) . liftIO . F.pixelGetGreen

getGreenQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getGreenQuantum =  liftIO . F.pixelGetGreenQuantum

setBlueQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setBlueQuantum = (liftIO .) . F.pixelSetBlueQuantum

getBlue :: (MonadResource m) => PPixelWand -> m Double
getBlue = (fmap realToFrac) . liftIO . F.pixelGetBlue

getBlueQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getBlueQuantum =  liftIO . F.pixelGetBlueQuantum
