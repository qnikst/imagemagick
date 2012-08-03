{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.Vector.Storable            as V
import           Filesystem.Path.CurrentOS
import           Graphics.ImageMagick.MagickWand
import           System.Environment

{- Example application for making contrast image
 - Example is taken from imagemagick documentation
 -}
main = do
  [img, img'] <- getArgs
  withMagickWandGenesis $ do
    (_,w) <- magickWand
    tR <- readImage w $ decodeString img
    -- unless tR throw
    (_,w')  <- cloneMagickWand w
    (_,it)  <- pixelIterator w
    (_,it') <- pixelIterator w'
    h   <- getImageHeight w
    forM_ [1..h] $ \y -> do
      (_,pixels)  <- pixelGetNextIteratorRow it
      (_,pixels') <- pixelGetNextIteratorRow it'
      V.zipWithM_ (contrast it') pixels pixels'
      writeImages w' (decodeString img') True
  where
    contrast :: (MonadResource m) => PPixelIterator -> PPixelWand -> PPixelWand -> m Bool
    contrast i p p' = do
        c <- pixelGetMagickColor p
        setPixelRed   c =<< fmap sigmoidalContrast (getPixelRed c)
        setPixelGreen c =<< fmap sigmoidalContrast (getPixelGreen c)
        setPixelBlue  c =<< fmap sigmoidalContrast (getPixelBlue c)
        setPixelIndex c =<< fmap sigmoidalContrast (getPixelIndex c)
        pixelSetMagickColor p' c
        pixelSyncIterator i
    quantumScale :: MagickRealType
    quantumScale = 1 / quantumRange
    sigmoidalContrast :: MagickRealType -> MagickRealType
    sigmoidalContrast x = (quantumRange/(1+ exp (10.0*(0.5-quantumScale*x) ))-0.0066928509)*1.0092503


