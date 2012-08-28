{-# LANGUAGE CPP #-}
module Graphics.ImageMagick.MagickWand.PixelWand
  ( pixelWand
-- , clearPixelWand 
--   , cloneWand 
--   , cloneWands 
--   , isWandSimilar 
--   , isWand 
   , setColorCount, getColorCount 
  -- ** Literal names
   , setColor
   , getColorAsString, getColorAsNormalizedString 
   -- HSL
   , getHSL, setHSL 
   , getMagickColor, setMagickColor 
   , setColorFromWand 
   , getQuantumColor, setQuantumColor 
   -- ** Color parts
   -- Index
   , getIndex, setIndex 
   -- Fuzz
   , getFuzz, setFuzz 
   -- Alpha
   , getOpacity, getOpacityQuantum, setOpacity, setOpacityQuantum 
   , getAlpha, getAlphaQuantum, setAlpha, setAlphaQuantum 
   -- RGB
   , getRed, setRed, getRedQuantum, setRedQuantum, setBlueQuantum, getBlue, setBlue, getBlueQuantum
   , setGreenQuantum, getGreen, getGreenQuantum, setGreen, setGreenQuantum 
   -- CMYK
   , getCyan, getCyanQuantum, setCyan, setCyanQuantum, getMagenta, getMagentaQuantum, setMagenta
   , setMagentaQuantum, getYellow, getYellowQuantum, setYellow, setYellowQuantum, getBlack, getBlackQuantum 
   , setBlack, setBlackQuantum 
  ) where

import           Control.Monad                                 (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                               (ByteString, useAsCString, packCString)
import           Foreign                                       hiding (void)

import           Graphics.ImageMagick.MagickWand.FFI.PixelWand as F
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils

pixelWand :: (MonadResource m) => m PPixelWand
pixelWand = fmap snd (allocate F.newPixelWand destroy)
  where destroy = void . F.destroyPixelWand

setColor :: (MonadResource m) => PPixelWand -> ByteString -> m ()
setColor p s = withException_ p $ useAsCString s (F.pixelSetColor p)


getMagickColor :: (MonadResource m) => PPixelWand -> m PMagickPixelPacket
getMagickColor w = liftIO $ do
  p <- mallocForeignPtr
  withForeignPtr p (F.pixelGetMagickColor w)
  return p

setMagickColor :: (MonadResource m) => PPixelWand -> PMagickPixelPacket -> m ()
setMagickColor w p = liftIO $ withForeignPtr p (F.pixelSetMagickColor w)

setColorCount :: (MonadResource m) => PPixelWand -> Int -> m ()
setColorCount w i = liftIO $ F.pixelSetColorCount w (fromIntegral i)

getColorCount :: (MonadResource m) => PPixelWand -> m Int
getColorCount w = liftIO (F.pixelGetColorCount w) >>= return . fromIntegral

getColorAsString :: (MonadResource m) => PPixelWand -> m ByteString
getColorAsString w = liftIO $ F.pixelGetColorAsString w >>= packCString 

getColorAsNormalizedString :: (MonadResource m) => PPixelWand -> m ByteString
getColorAsNormalizedString w = liftIO $ F.pixelGetColorAsNormalizedString w >>= packCString 

getHSL :: (MonadResource m) => PPixelWand -> m (Double, Double, Double)
getHSL w = liftIO $ fmap (map3 realToFrac) (with3 (F.pixelGetHSL w))

setHSL :: (MonadResource m) => PPixelWand -> Double -> Double -> Double -> m ()
setHSL w h s l = liftIO $ F.pixelSetHSL w (realToFrac h) (realToFrac s) (realToFrac l)

setColorFromWand :: (MonadResource m) => PPixelWand -> PPixelWand -> m ()
setColorFromWand = (liftIO .). F.pixelSetColorFromWand

getIndex :: (MonadResource m) => PPixelWand -> m IndexPacket
getIndex = liftIO . ((fmap fromIntegral) . F.pixelGetIndex)

setIndex :: (MonadResource m) => PPixelWand -> IndexPacket -> m ()
setIndex w i = liftIO $ F.pixelSetIndex w (fromIntegral i)

getQuantumColor :: (MonadResource m) => PPixelWand -> m PPixelPacket 
getQuantumColor w = liftIO $ do
  p <- mallocForeignPtr
  withForeignPtr p (F.pixelGetQuantumColor w)
  return p

setQuantumColor :: (MonadResource m) => PPixelWand -> PPixelPacket -> m ()
setQuantumColor w p = liftIO $ withForeignPtr p (F.pixelSetQuantumColor w)

getFuzz :: (MonadResource m) => PPixelWand -> m Double 
getFuzz = liftIO . ((fmap realToFrac) . F.pixelGetFuzz)

setFuzz :: (MonadResource m) => PPixelWand -> Double -> m ()
setFuzz w i = liftIO $ F.pixelSetFuzz w (realToFrac i)


setRedQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setRedQuantum = (liftIO .) . F.pixelSetRedQuantum

getRed :: (MonadResource m) => PPixelWand -> m Double
getRed = (fmap realToFrac) . liftIO . F.pixelGetRed

setRed :: (MonadResource m) => PPixelWand -> Double -> m ()
setRed = (liftIO .) . (. realToFrac) . F.pixelSetRed

getRedQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getRedQuantum =  liftIO . F.pixelGetRedQuantum

setGreenQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setGreenQuantum = (liftIO .) . F.pixelSetGreenQuantum

getGreen :: (MonadResource m) => PPixelWand -> m Double
getGreen = (fmap realToFrac) . liftIO . F.pixelGetGreen

setGreen :: (MonadResource m) => PPixelWand -> Double -> m ()
setGreen = (liftIO .) . (. realToFrac) . F.pixelSetGreen

getGreenQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getGreenQuantum =  liftIO . F.pixelGetGreenQuantum

setBlueQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setBlueQuantum = (liftIO .) . F.pixelSetBlueQuantum

getBlue :: (MonadResource m) => PPixelWand -> m Double
getBlue = (fmap realToFrac) . liftIO . F.pixelGetBlue

setBlue :: (MonadResource m) => PPixelWand -> Double -> m ()
setBlue = (liftIO .) . (. realToFrac) . F.pixelSetBlue

getBlueQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getBlueQuantum =  liftIO . F.pixelGetBlueQuantum

setAlphaQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setAlphaQuantum = (liftIO .) . F.pixelSetAlphaQuantum

getAlphaQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getAlphaQuantum =  liftIO . F.pixelGetAlphaQuantum

setAlpha :: (MonadResource m) => PPixelWand -> Double -> m ()
setAlpha = (liftIO .) . (. realToFrac) . F.pixelSetAlpha

getAlpha :: (MonadResource m) => PPixelWand -> m Double
getAlpha = (fmap realToFrac) . liftIO . F.pixelGetAlpha

setOpacityQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setOpacityQuantum = (liftIO .) . F.pixelSetOpacityQuantum

getOpacityQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getOpacityQuantum =  liftIO . F.pixelGetOpacityQuantum

setOpacity :: (MonadResource m) => PPixelWand -> Double -> m ()
setOpacity = (liftIO .) . (. realToFrac) . F.pixelSetOpacity

getOpacity :: (MonadResource m) => PPixelWand -> m Double
getOpacity = (fmap realToFrac) . liftIO . F.pixelGetOpacity

setBlackQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setBlackQuantum = (liftIO .) . F.pixelSetBlackQuantum

getBlackQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getBlackQuantum =  liftIO . F.pixelGetBlackQuantum

setBlack :: (MonadResource m) => PPixelWand -> Double -> m ()
setBlack = (liftIO .) . (. realToFrac) . F.pixelSetBlack

getBlack :: (MonadResource m) => PPixelWand -> m Double
getBlack = (fmap realToFrac) . liftIO . F.pixelGetBlack

setCyanQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setCyanQuantum = (liftIO .) . F.pixelSetCyanQuantum

getCyanQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getCyanQuantum =  liftIO . F.pixelGetCyanQuantum

setCyan :: (MonadResource m) => PPixelWand -> Double -> m ()
setCyan = (liftIO .) . (. realToFrac) . F.pixelSetCyan

getCyan :: (MonadResource m) => PPixelWand -> m Double
getCyan = (fmap realToFrac) . liftIO . F.pixelGetCyan

setMagentaQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setMagentaQuantum = (liftIO .) . F.pixelSetMagentaQuantum

getMagentaQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getMagentaQuantum =  liftIO . F.pixelGetMagentaQuantum

setMagenta :: (MonadResource m) => PPixelWand -> Double -> m ()
setMagenta = (liftIO .) . (. realToFrac) . F.pixelSetMagenta 

getMagenta :: (MonadResource m) => PPixelWand -> m Double
getMagenta = (fmap realToFrac) . liftIO . F.pixelGetMagenta

setYellowQuantum :: (MonadResource m) => PPixelWand -> Quantum -> m ()
setYellowQuantum = (liftIO .) . F.pixelSetYellowQuantum

getYellowQuantum :: (MonadResource m) => PPixelWand -> m Quantum
getYellowQuantum =  liftIO . F.pixelGetYellowQuantum

setYellow :: (MonadResource m) => PPixelWand -> Double -> m ()
setYellow = (liftIO .) . (. realToFrac) . F.pixelSetYellow

getYellow :: (MonadResource m) => PPixelWand -> m Double
getYellow = (fmap realToFrac) . liftIO . F.pixelGetYellow

---
with3 f = alloca (\x -> alloca (\y -> alloca (\z -> do 
              f x y z 
              x' <- peek x
              y' <- peek y
              z' <- peek z
              return (x',y',z')
              )))
map3 f (a,b,c) = (f a, f b, f c)
