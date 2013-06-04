module Graphics.ImageMagick.MagickCore.Gem
  ( convertHSBToRGB
  , convertHSLToRGB
  , convertHWBToRGB
  , convertRGBToHSB
  , convertRGBToHSL
  , convertRGBToHWB
  ) where

import Foreign.Ptr              (Ptr)
import Foreign.Storable         (Storable, peek)
import Foreign.Marshal.Alloc    (alloca)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Graphics.ImageMagick.MagickCore.Types
import qualified Graphics.ImageMagick.MagickCore.FFI.Gem as F


with3 :: (Storable a, Storable b, Storable c) =>
         (Ptr a -> Ptr b -> Ptr c -> IO ())
       -> IO (a, b, c)
with3 f = alloca (\x -> alloca (\y -> alloca (\z -> do 
              f x y z 
              x' <- peek x
              y' <- peek y
              z' <- peek z
              return (x',y',z')
              )))

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (a,b,c) = (f a, f b, f c)


convertHSBToRGB :: MonadResource m => Double -> Double -> Double -> m (Quantum, Quantum, Quantum)
convertHSBToRGB d1 d2 d3 = liftIO $ with3 (F.convertHSBToRGB (realToFrac d1) (realToFrac d2) (realToFrac d3))

convertHSLToRGB :: MonadResource m => Double -> Double -> Double -> m (Quantum, Quantum, Quantum)
convertHSLToRGB d1 d2 d3 = liftIO $ with3 (F.convertHSLToRGB (realToFrac d1) (realToFrac d2) (realToFrac d3))

convertHWBToRGB :: MonadResource m => Double -> Double -> Double -> m (Quantum, Quantum, Quantum)
convertHWBToRGB d1 d2 d3 = liftIO $ with3 (F.convertHWBToRGB (realToFrac d1) (realToFrac d2) (realToFrac d3))

convertRGBToHSB :: MonadResource m => Quantum -> Quantum -> Quantum -> m (Double, Double, Double)
convertRGBToHSB q1 q2 q3 = (liftIO $ with3 (F.convertRGBToHSB q1 q2 q3)) >>= return . (map3 realToFrac)

convertRGBToHSL :: MonadResource m => Quantum -> Quantum -> Quantum -> m (Double, Double, Double)
convertRGBToHSL q1 q2 q3 = liftIO $ with3 (F.convertRGBToHSL q1 q2 q3) >>= return . (map3 realToFrac)

convertRGBToHWB :: MonadResource m => Quantum -> Quantum -> Quantum -> m (Double, Double, Double)
convertRGBToHWB q1 q2 q3 = liftIO $ with3 (F.convertRGBToHSB q1 q2 q3) >>= return . (map3 realToFrac)

