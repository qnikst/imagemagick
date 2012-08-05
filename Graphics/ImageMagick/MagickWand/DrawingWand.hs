module Graphics.ImageMagick.MagickWand.DrawingWand
  ( drawingWand
  , getFillColor
  , setFillColor
  , drawRoundRectangle
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Foreign
import           Foreign.C.Types
import           Graphics.ImageMagick.MagickWand.FFI.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.DrawingWand as F
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils

drawingWand :: (MonadResource m) => m (ReleaseKey, PDrawingWand)
drawingWand = allocate (F.newDrawingWand) (void . F.destroyDrawingWand)

-- | returns the fill color used for drawing filled objects.
getFillColor :: (MonadResource m) => PDrawingWand -> PPixelWand -> m ()
getFillColor = (liftIO .). F.drawGetFillColor


-- | DrawSetFillColor() sets the fill color to be used for drawing filled objects.
setFillColor :: (MonadResource m) => PDrawingWand -> PPixelWand -> m ()
setFillColor = (liftIO .). F.drawSetFillColor

-- | DrawRoundRectangle() draws a rounted rectangle given two coordinates, 
--   x & y corner radiuses and using the current stroke, stroke width, and fill settings.
drawRoundRectangle :: (MonadResource m) => PDrawingWand 
     -> Double           -- ^ x ordinate of first coordinate
     -> Double           -- ^ y ordinate of first coordinate
     -> Double           -- ^ x ordinate of second coordinate
     -> Double           -- ^ y ordinate of second coordinate
     -> Double           -- ^ radius of corner in horizontal direction
     -> Double           -- ^ radius of corner in vertical direction
     -> m ()
drawRoundRectangle p x1 y1 x2 y2 rx ry = liftIO $ F.drawRoundRectangle p (realToFrac x1)
                                                                         (realToFrac y1)
                                                                         (realToFrac x2)
                                                                         (realToFrac y2)
                                                                         (realToFrac rx)
                                                                         (realToFrac ry)
