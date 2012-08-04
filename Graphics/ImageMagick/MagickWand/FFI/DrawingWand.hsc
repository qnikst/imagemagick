{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickWand.FFI.DrawingWand
  where

import Foreign
import Foreign.C.Types
import Graphics.ImageMagick.MagickWand.FFI.Types

#include <wand/MagickWand.h>

-- | NewDrawingWand() returns a drawing wand required for all other methods in the API.
foreign import ccall "NewDrawingWand" newDrawingWand
  :: IO (Ptr DrawingWand)

-- | DestroyDrawingWand() frees all resources associated with the drawing wand. 
-- Once the drawing wand has been freed, it should not be used and further unless it re-allocated.
foreign import ccall "DestroyDrawingWand" destroyDrawingWand
  :: Ptr DrawingWand -> IO (Ptr DrawingWand)

-- | DrawGetFillColor() returns the fill color used for drawing filled objects.
foreign import ccall "DrawGetFillColor" drawGetFillColor
  :: Ptr DrawingWand -> Ptr PixelWand -> IO ()

-- | DrawSetFillColor() sets the fill color to be used for drawing filled objects.
foreign import ccall "DrawSetFillColor" drawSetFillColor
  :: Ptr DrawingWand -> Ptr PixelWand -> IO ()

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


