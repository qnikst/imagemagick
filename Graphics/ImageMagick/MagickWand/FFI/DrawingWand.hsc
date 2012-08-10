{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickWand.FFI.DrawingWand
  where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.Types

#include <wand/MagickWand.h>

-- | NewDrawingWand() returns a drawing wand required for all other methods in the API.
foreign import ccall "NewDrawingWand" newDrawingWand
  :: IO (Ptr DrawingWand)

-- | DestroyDrawingWand() frees all resources associated with the drawing wand.
-- Once the drawing wand has been freed, it should not be used and further unless it re-allocated.
foreign import ccall "DestroyDrawingWand" destroyDrawingWand
  :: Ptr DrawingWand -> IO (Ptr DrawingWand)

-- | PixelGetException() returns the severity, reason, and description of any
--   error that occurs when using other methods in this API.
foreign import ccall "DrawGetException" drawGetException
  :: Ptr DrawingWand -> Ptr ExceptionType -> IO CString

-- | DrawGetFillColor() returns the fill color used for drawing filled objects.
foreign import ccall "DrawGetFillColor" drawGetFillColor
  :: Ptr DrawingWand -> Ptr PixelWand -> IO ()

-- | DrawSetFillColor() sets the fill color to be used for drawing filled objects.
foreign import ccall "DrawSetFillColor" drawSetFillColor
  :: Ptr DrawingWand -> Ptr PixelWand -> IO ()

-- | DrawSetFillPatternURL() sets the URL to use as a fill pattern
-- for filling objects. Only local URLs ("#identifier") are supported
-- at this time. These local URLs are normally created by defining a named
-- fill pattern with DrawPushPattern/DrawPopPattern.
foreign import ccall "DrawSetFillPatternURL" drawSetFillPatternURL
  :: Ptr DrawingWand -> CString -> IO MagickBooleanType

-- | DrawSetFillRule() sets the fill rule to use while drawing polygons.
foreign import ccall "DrawSetFillRule" drawSetFillRule
  :: Ptr DrawingWand -> FillRule -> IO ()

-- | DrawSetFont() sets the fully-sepecified font to use when annotating with text.
foreign import ccall "DrawSetFont" drawSetFont
  :: Ptr DrawingWand -> CString -> IO ()

-- | DrawSetFontSize() sets the font pointsize to use when annotating with text.
foreign import ccall "DrawSetFontSize" drawSetFontSize
  :: Ptr DrawingWand -> CDouble -> IO ()

-- | DrawSetGravity() sets the text placement gravity to use when annotating with text.
foreign import ccall "DrawSetGravity" drawSetGravity
  :: Ptr DrawingWand -> GravityType -> IO ()

-- | DrawSetStrokeAntialias() controls whether stroked outlines are antialiased.
-- Stroked outlines are antialiased by default. When antialiasing is disabled
-- stroked pixels are thresholded to determine if the stroke color or
-- underlying canvas color should be used.
foreign import ccall "DrawSetStrokeAntialias" drawSetStrokeAntialias
  :: Ptr DrawingWand
  -> MagickBooleanType    -- ^ stroke_antialias
  -> IO ()

-- | DrawSetStrokeColor() sets the color used for stroking object outlines.
foreign import ccall "DrawSetStrokeColor" drawSetStrokeColor
  :: Ptr DrawingWand
  -> Ptr PixelWand        -- ^ stroke_wand
  -> IO ()

-- |  DrawSetStrokeDashArray() specifies the pattern of dashes and gaps used to
-- stroke paths. The stroke dash array represents an array of numbers that
-- specify the lengths of alternating dashes and gaps in pixels. If an odd
-- number of values is provided, then the list of values is repeated to yield
-- an even number of values. To remove an existing dash array, pass a zero
-- number_elements argument and null dash_array.  A typical stroke dash array
-- might contain the members 5 3 2.
foreign import ccall "DrawSetStrokeDashArray" drawSetStrokeDashArray
  :: Ptr DrawingWand
  -> CSize             -- ^ number of elements in dash array
  -> Ptr CDouble       -- ^ dash array values
  -> IO ()

-- | DrawSetStrokeLineCap() specifies the shape to be used at the end
-- of open subpaths when they are stroked. Values of LineCap are UndefinedCap,
-- ButtCap, RoundCap, and SquareCap.
foreign import ccall "DrawSetStrokeLineCap" drawSetStrokeLineCap
  :: Ptr DrawingWand
  -> LineCap           -- ^ linecap
  -> IO ()

-- | DrawSetStrokeLineJoin() specifies the shape to be used at the corners
-- of paths (or other vector shapes) when they are stroked.
-- Values of LineJoin are UndefinedJoin, MiterJoin, RoundJoin, and BevelJoin.
foreign import ccall "DrawSetStrokeLineJoin" drawSetStrokeLineJoin
  :: Ptr DrawingWand
  -> LineJoin          -- ^ linejoin
  -> IO ()

-- | DrawSetStrokeOpacity() specifies the opacity of stroked object outlines.
foreign import ccall "DrawSetStrokeOpacity" drawSetStrokeOpacity
  :: Ptr DrawingWand
  -> CDouble           -- ^ stroke_opacity
  -> IO ()

-- | DrawSetStrokeOpacity() specifies the opacity of stroked object outlines.
foreign import ccall "DrawSetTextAntialias" drawSetTextAntialias
  :: Ptr DrawingWand
  -> MagickBooleanType -- ^ antialias boolean. Set to false (0) to disable antialiasing.
  -> IO ()

-- | DrawSetStrokeWidth() sets the width of the stroke used to draw object outlines.
foreign import ccall "DrawSetStrokeWidth" drawSetStrokeWidth
  :: Ptr DrawingWand
  -> CDouble           -- ^ stroke_width
  -> IO ()

-- | DrawAnnotation() draws text on the image.
foreign import ccall "DrawAnnotation" drawAnnotation
  :: Ptr DrawingWand
  -> CDouble           -- ^ x ordinate to left of text
  -> CDouble           -- ^ y ordinate to text baseline
  -> CString           -- ^ text to draw
  -> IO ()

-- | DrawCircle() draws a circle on the image.
foreign import ccall "DrawCircle" drawCircle
  :: Ptr DrawingWand
  -> CDouble           -- ^ origin x ordinate
  -> CDouble           -- ^ origin y ordinate
  -> CDouble           -- ^ perimeter x ordinate
  -> CDouble           -- ^ perimeter y ordinate
  -> IO ()

-- | DrawComposite() composites an image onto the current image, using
-- the specified composition operator, specified position, and at the specified size.
foreign import ccall "DrawComposite" drawComposite
  :: Ptr DrawingWand
  -> CompositeOperator -- ^ composition operator
  -> CDouble           -- ^ x ordinate of top left corner
  -> CDouble           -- ^ y ordinate of top left corner
  -> CDouble           -- ^ width to resize image to prior to compositing, specify zero to use existing width
  -> CDouble           -- ^ height to resize image to prior to compositing, specify zero to use existing height
  -> Ptr MagickWand    -- ^ image to composite is obtained from this wand
  -> IO MagickBooleanType


-- | DrawEllipse() draws an ellipse  on the image.
foreign import ccall "DrawEllipse" drawEllipse
  :: Ptr DrawingWand
  -> CDouble           -- ^ origin x ordinate
  -> CDouble           -- ^ origin y ordinate
  -> CDouble           -- ^ radius in x
  -> CDouble           -- ^ radius in y
  -> CDouble           -- ^ starting rotation in degrees
  -> CDouble           -- ^ ending rotation in degrees
  -> IO ()

-- | DrawLine() draws a line on the image using the current stroke color,
-- stroke opacity, and stroke width.
foreign import ccall "DrawLine" drawLine
  :: Ptr DrawingWand
  -> CDouble           -- ^ starting x ordinate
  -> CDouble           -- ^ starting y ordinate
  -> CDouble           -- ^ ending x ordinate
  -> CDouble           -- ^ ending y ordinate
  -> IO ()

-- | DrawPolygon() draws a polygon using the current stroke, stroke width,
-- and fill color or texture, using the specified array of coordinates.
foreign import ccall "DrawPolygon" drawPolygon
  :: Ptr DrawingWand
  -> CSize           -- ^ number of coordinates
  -> Ptr PointInfo   -- ^ coordinate array
  -> IO ()

-- | DrawRectangle() draws a rectangle given two coordinates
-- and using the current stroke, stroke width, and fill settings.
foreign import ccall "DrawRectangle" drawRectangle
  :: Ptr DrawingWand
  -> CDouble           -- ^ x ordinate of first coordinate
  -> CDouble           -- ^ y ordinate of first coordinate
  -> CDouble           -- ^ x ordinate of second coordinate
  -> CDouble           -- ^ y ordinate of second coordinate
  -> IO ()

-- | DrawRoundRectangle() draws a rounted rectangle given two coordinates,
-- x & y corner radiuses and using the current stroke, stroke width, and fill settings.
foreign import ccall "DrawRoundRectangle" drawRoundRectangle
  :: Ptr DrawingWand
  -> CDouble           -- ^ x ordinate of first coordinate
  -> CDouble           -- ^ y ordinate of first coordinate
  -> CDouble           -- ^ x ordinate of second coordinate
  -> CDouble           -- ^ y ordinate of second coordinate
  -> CDouble           -- ^ radius of corner in horizontal direction
  -> CDouble           -- ^ radius of corner in vertical direction
  -> IO ()

-- | PushDrawingWand() clones the current drawing wand to create a new drawing wand.
-- The original drawing wand(s) may be returned to by invoking PopDrawingWand().
-- The drawing wands are stored on a drawing wand stack. For every Pop there must
-- have already been an equivalent Push.
foreign import ccall "PushDrawingWand" pushDrawingWand
  :: Ptr DrawingWand
  -> IO MagickBooleanType

-- | PopDrawingWand() destroys the current drawing wand and returns to the
-- previously pushed drawing wand. Multiple drawing wands may exist.
-- It is an error to attempt to pop more drawing wands than have been pushed,
-- and it is proper form to pop all drawing wands which have been pushed.
foreign import ccall "PopDrawingWand" popDrawingWand
  :: Ptr DrawingWand
  -> IO MagickBooleanType

-- | DrawRotate() applies the specified rotation to the current coordinate space.
foreign import ccall "DrawRotate" drawRotate
  :: Ptr DrawingWand
  -> CDouble           -- ^ degrees of rotation
  -> IO ()

-- | DrawTranslate() applies a translation to the current coordinate system
-- which moves the coordinate system origin to the specified coordinate.
foreign import ccall "DrawTranslate" drawTranslate
  :: Ptr DrawingWand
  -> CDouble           -- ^ new x ordinate for coordinate system origin
  -> CDouble           -- ^ new y ordinate for coordinate system origin
  -> IO ()

-- | DrawPushPattern() indicates that subsequent commands up to
-- a DrawPopPattern() command comprise the definition of a named pattern.
-- The pattern space is assigned top left corner coordinates, a width and height,
-- and becomes its own drawing space. Anything which can be drawn may be used
-- in a pattern definition. Named patterns may be used as stroke or brush definitions.
foreign import ccall "DrawPushPattern" drawPushPattern
  :: Ptr DrawingWand
  -> CString           -- ^ pattern identification for later reference
  -> CDouble           -- x ordinate of top left corner
  -> CDouble           -- y ordinate of top left corner
  -> CDouble           -- width of pattern space
  -> CDouble           -- height of pattern space
  -> IO MagickBooleanType

-- | DrawPopPattern() terminates a pattern definition.
foreign import ccall "DrawPopPattern" drawPopPattern
  :: Ptr DrawingWand
  -> IO MagickBooleanType


-- | DrawColor() draws color on image using the current fill color, starting at 
-- specified position, and using specified paint method. The available paint methods are:
--
--    PointMethod: Recolors the target pixel
--    ReplaceMethod: Recolor any pixel that matches the target pixel.
--    FloodfillMethod: Recolors target pixels and matching neighbors.
--    ResetMethod: Recolor all pixels.
foreign import ccall "DrawColor" drawColor
  :: Ptr DrawingWand
  -> CDouble
  -> CDouble
  -> PaintMethod
  -> IO ()
