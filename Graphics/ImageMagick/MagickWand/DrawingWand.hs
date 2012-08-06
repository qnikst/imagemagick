module Graphics.ImageMagick.MagickWand.DrawingWand
  ( drawingWand
  , getFillColor
  , setFillColor
  , setFillRule
  , setStrokeAntialias
  , setStrokeColor
  , setStrokeDashArray
  , setStrokeLineCap
  , setStrokeLineJoin
  , setStrokeOpacity
  , setStrokeWidth
  , drawCircle
  , drawEllipse
  , drawLine
  , drawPolygon
  , drawRectangle
  , drawRoundRectangle
  , pushDrawingWand
  , popDrawingWand
  , rotate
  , translate
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Foreign                                         hiding (rotate)
import           Foreign.C.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.DrawingWand as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
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

-- | Sets the fill rule to use while drawing polygons.
setFillRule :: (MonadResource m) => PDrawingWand -> FillRule -> m ()
setFillRule = (liftIO .). F.drawSetFillRule

setStrokeAntialias :: (MonadResource m) => PDrawingWand -> Bool -> m ()
setStrokeAntialias dw antialias = liftIO $ F.drawSetStrokeAntialias dw (toMBool antialias)

-- | sets the color used for stroking object outlines.
setStrokeColor :: (MonadResource m) => PDrawingWand -> PPixelWand -> m ()
setStrokeColor = (liftIO .). F.drawSetStrokeColor

-- | Specifies the pattern of dashes and gaps used to
-- stroke paths. The stroke dash array represents an array of numbers that
-- specify the lengths of alternating dashes and gaps in pixels. If an odd
-- number of values is provided, then the list of values is repeated to yield
-- an even number of values. To remove an existing dash array, pass an emtpy list.
-- A typical stroke dash array might contain the members 5 3 2.
setStrokeDashArray :: (MonadResource m) => PDrawingWand -> [Double] -> m ()
setStrokeDashArray dw [] = liftIO $ F.drawSetStrokeDashArray dw 0 nullPtr
setStrokeDashArray dw dashes = liftIO $ withArray (map realToFrac dashes) $ \arr ->
  F.drawSetStrokeDashArray dw (fromIntegral $ length dashes) arr

-- | Specifies the shape to be used at the end of open subpaths
-- when they are stroked. Values of `LineCap` are `undefinedCap`,
-- `buttCap, `roundCap` and `squareCap`.
setStrokeLineCap :: (MonadResource m) => PDrawingWand -> LineCap -> m ()
setStrokeLineCap = (liftIO .). F.drawSetStrokeLineCap

-- | Specifies the shape to be used at the corners of paths
-- (or other vector shapes) when they are stroked.
-- Values of `LineJoin` are `undefinedJoin`, `miterJoin`, `roundJoin` and `bevelJoin`.
setStrokeLineJoin :: (MonadResource m) => PDrawingWand -> LineJoin -> m ()
setStrokeLineJoin = (liftIO .). F.drawSetStrokeLineJoin

-- | specifies the opacity of stroked object outlines.
setStrokeOpacity :: (MonadResource m) => PDrawingWand -> Double -> m ()
setStrokeOpacity dw op = liftIO $ F.drawSetStrokeOpacity dw (realToFrac op)

-- | sets the width of the stroke used to draw object outlines.
setStrokeWidth :: (MonadResource m) => PDrawingWand -> Double -> m ()
setStrokeWidth dw width = liftIO $ F.drawSetStrokeWidth dw (realToFrac width)

-- | Draws a circle on the image.
drawCircle :: (MonadResource m) => PDrawingWand
     -> Double           -- ^ origin x ordinate
     -> Double           -- ^ origin y ordinate
     -> Double           -- ^ perimeter x ordinate
     -> Double           -- ^ perimeter y ordinate
     -> m ()
drawCircle dw ox oy px py = liftIO $ F.drawCircle dw (realToFrac ox) (realToFrac oy)
                                                     (realToFrac px) (realToFrac py)

-- | Draws an ellipse on the image.
drawEllipse :: (MonadResource m) => PDrawingWand
     -> Double           -- ^ origin x ordinate
     -> Double           -- ^ origin y ordinate
     -> Double           -- ^ radius in x
     -> Double           -- ^ radius in y
     -> Double           -- ^ starting rotation in degrees
     -> Double           -- ^ ending rotation in degrees
     -> m ()
drawEllipse dw ox oy rx ry start end = liftIO $ F.drawEllipse dw (realToFrac ox) (realToFrac oy)
                                                                 (realToFrac rx) (realToFrac ry)
                                                                 (realToFrac start) (realToFrac end)

-- | Draws a line on the image using the current stroke color,
-- stroke opacity, and stroke width.
drawLine :: (MonadResource m) => PDrawingWand
  -> Double           -- ^ starting x ordinate
  -> Double           -- ^ starting y ordinate
  -> Double           -- ^ ending x ordinate
  -> Double           -- ^ ending y ordinate
  -> m ()
drawLine dw sx sy ex ey = liftIO $ F.drawLine dw (realToFrac sx) (realToFrac sy)
                                                 (realToFrac ex) (realToFrac ey)

-- | Draws a polygon using the current stroke, stroke width,
-- and fill color or texture, using the specified array of coordinates.
drawPolygon :: (MonadResource m) => PDrawingWand
     -> [PointInfo]      -- ^ coordinates
     -> m ()
drawPolygon dw points = liftIO $ withArrayLen points $ \len arr ->
  F.drawPolygon dw (fromIntegral len) arr

-- | Draws a rectangle given two coordinates
-- and using the current stroke, stroke width, and fill settings.
drawRectangle :: (MonadResource m) => PDrawingWand
     -> Double           -- ^ x ordinate of first coordinate
     -> Double           -- ^ y ordinate of first coordinate
     -> Double           -- ^ x ordinate of second coordinate
     -> Double           -- ^ y ordinate of second coordinate
     -> m ()
drawRectangle dw x1 y1 x2 y2 = liftIO $ F.drawRectangle dw (realToFrac x1) (realToFrac y1)
                                                           (realToFrac x2) (realToFrac y2)

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
-- | Clones the current drawing wand to create a new drawing wand.
-- The original drawing wand(s) may be returned to by invoking `popDrawingWand`.
-- The drawing wands are stored on a drawing wand stack. For every Pop there must
-- have already been an equivalent Push.
pushDrawingWand ::  (MonadResource m) => PDrawingWand -> m ()
pushDrawingWand dw = withException_ dw $ F.pushDrawingWand dw

-- | Destroys the current drawing wand and returns to the
-- previously pushed drawing wand. Multiple drawing wands may exist.
-- It is an error to attempt to pop more drawing wands than have been pushed,
-- and it is proper form to pop all drawing wands which have been pushed.
popDrawingWand ::  (MonadResource m) => PDrawingWand -> m ()
popDrawingWand dw = withException_ dw $ F.popDrawingWand dw

-- | Applies the specified rotation to the current coordinate space.
rotate ::  (MonadResource m) => PDrawingWand -> Double -> m ()
rotate dw degrees = liftIO $ F.drawRotate dw (realToFrac degrees)

-- | Applies a translation to the current coordinate system
-- which moves the coordinate system origin to the specified coordinate.
translate :: (MonadResource m) => PDrawingWand -> Double -> Double -> m ()
translate dw x y = liftIO $ F.drawTranslate dw (realToFrac x) (realToFrac y)
