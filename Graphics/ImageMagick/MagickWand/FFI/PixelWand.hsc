{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickWand.FFI.PixelWand
  where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.Types

#include <wand/MagickWand.h>

-- | DestroyPixelWand() deallocates resources associated with a PixelWand.

foreign import ccall "DestroyPixelWand" destroyPixelWand
  :: Ptr PixelWand -> IO (Ptr PixelWand)

foreign import ccall "DestroyPixelWands" destroyPixelWands
  :: Ptr PixelWand -> CSize -> IO ()

foreign import ccall "IsPixelWand" isPixelWand
  :: Ptr PixelWand -> IO MagickBooleanType

-- | PixelGetMagickColor() gets the magick color of the pixel wand.
foreign import ccall "PixelGetMagickColor" pixelGetMagickColor
  :: Ptr PixelWand -> Ptr MagickPixelPacket -> IO ()

-- | PixelSetMagickColor() sets the color of the pixel wand.
foreign import ccall "PixelSetMagickColor" pixelSetMagickColor
  :: Ptr PixelWand -> Ptr MagickPixelPacket -> IO ()

foreign import ccall "ClearPixelWand" clearPixelWand
  :: Ptr PixelWand -> IO ()

foreign import ccall "ClonePixelWand" clonePixelWand
  :: Ptr PixelWand -> IO (Ptr PixelWand)

-- | NewPixelWand() returns a new pixel wand.
foreign import ccall "NewPixelWand" newPixelWand
  :: IO (Ptr PixelWand)

-- | NewPixelWands() returns an array of pixel wands.
foreign import ccall "NewPixelWands" newPixelWands
  :: CSize -> IO (Ptr (Ptr PixelWand))

-- | PixelSetColor() sets the color of the pixel wand with a string (e.g. "blue", "#0000ff", "rgb(0,0,255)", "cmyk(100,100,100,10)", etc.).
foreign import ccall "PixelSetColor" pixelSetColor
  :: Ptr PixelWand -> CString -> IO MagickBooleanType

-- | PixelClearException() clear any exceptions associated with the iterator.
foreign import ccall "PixelClearException" pixelClearException
  :: Ptr PixelWand -> IO MagickBooleanType

-- | PixelGetException() returns the severity, reason, and description of any
--   error that occurs when using other methods in this API.
foreign import ccall "PixelGetException" pixelGetException
  :: Ptr PixelWand -> Ptr ExceptionType -> IO CString

-- | PixelGetExceptionType() the exception type associated with the wand.
--   If no exception has occurred, UndefinedExceptionType is returned.
foreign import ccall "PixelGetExceptionType" pixelGetExceptionType
  :: Ptr PixelWand -> IO ExceptionType

-- | PixelGetColorAsString() returnsd the color of the pixel wand as a string.
foreign import ccall "PixelGetColorAsString" pixelGetColorAsString
  :: Ptr PixelWand -> IO CString

-- | PixelGetColorAsNormalizedString() returns the normalized color of the pixel wand as a string.
foreign import ccall "PixelGetColorAsNormalizedString" pixelGetColorAsNormalizedString
  :: Ptr PixelWand -> IO CString

-- | PixelGetRed) returns the normalized red color of the pixel wand.
foreign import ccall "PixelSetRed" pixelSetRed
  :: Ptr PixelWand -> CDouble -> IO ()

-- | PixelSetRedQuantum() sets the red color of the pixel wand.
foreign import ccall "PixelSetRedQuantum" pixelSetRedQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

-- | PixelGetRed) returns the normalized red color of the pixel wand.
foreign import ccall "PixelGetRed" pixelGetRed
  :: Ptr PixelWand -> IO CDouble

-- | PixelGetRedQuantum() returns the red color of the pixel wand.
foreign import ccall "PixelGetRedQuantum" pixelGetRedQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelGetGreen) returns the normalized green color of the pixel wand.
foreign import ccall "PixelGetGreen" pixelGetGreen
  :: Ptr PixelWand -> IO CDouble

-- | PixelGetGreenQuantum() returns the green color of the pixel wand.
foreign import ccall "PixelGetGreenQuantum" pixelGetGreenQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetGreen() sets the green color of the pixel wand.
foreign import ccall "PixelSetGreen" pixelSetGreen
  :: Ptr PixelWand -> CDouble -> IO ()

-- | PixelSetGreenQuantum() sets the green color of the pixel wand.
foreign import ccall "PixelSetGreenQuantum" pixelSetGreenQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

-- | PixelGetBlue() returns the normalized blue color of the pixel wand.
foreign import ccall "PixelGetBlue" pixelGetBlue
  :: Ptr PixelWand -> IO CDouble

foreign import ccall "PixelSetBlue" pixelSetBlue
  :: Ptr PixelWand -> CDouble -> IO ()

-- | PixelGetBlueQuantum() returns the blue color of the pixel wand.
foreign import ccall "PixelGetBlueQuantum" pixelGetBlueQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetBlueQuantum() sets the blue color of the pixel wand.
foreign import ccall "PixelSetBlueQuantum" pixelSetBlueQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

-- | IsPixelWandSimilar() returns MagickTrue if the distance between
-- two colors is less than the specified distance.
foreign import ccall "IsPixelWandSimilar" isPixelWandSimilar
  :: Ptr PixelWand -> Ptr PixelWand
     -> CDouble -- ^ any two colors that are less than or equal to this distance squared are consider similar
     -> IO MagickBooleanType

-- | PixelGetCyan) returns the normalized blue color of the pixel wand.
foreign import ccall "PixelGetCyan" pixelGetCyan
  :: Ptr PixelWand -> IO CDouble

foreign import ccall "PixelSetCyan" pixelSetCyan
  :: Ptr PixelWand -> CDouble -> IO ()

-- | PixelGetCyanQuantum() returns the blue color of the pixel wand.
foreign import ccall "PixelGetCyanQuantum" pixelGetCyanQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetCyanQuantum() sets the blue color of the pixel wand.
foreign import ccall "PixelSetCyanQuantum" pixelSetCyanQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

-- | PixelGetMagenta) returns the normalized blue color of the pixel wand.
foreign import ccall "PixelGetMagenta" pixelGetMagenta
  :: Ptr PixelWand -> IO CDouble

foreign import ccall "PixelSetMagenta" pixelSetMagenta
  :: Ptr PixelWand -> CDouble -> IO ()

-- | PixelGetMagentaQuantum() returns the blue color of the pixel wand.
foreign import ccall "PixelGetMagentaQuantum" pixelGetMagentaQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetMagentaQuantum() sets the blue color of the pixel wand.
foreign import ccall "PixelSetMagentaQuantum" pixelSetMagentaQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

-- | PixelGetYellow) returns the normalized blue color of the pixel wand.
foreign import ccall "PixelGetYellow" pixelGetYellow
  :: Ptr PixelWand -> IO CDouble

foreign import ccall "PixelSetYellow" pixelSetYellow
  :: Ptr PixelWand -> CDouble -> IO ()

-- | PixelGetYellowQuantum() returns the blue color of the pixel wand.
foreign import ccall "PixelGetYellowQuantum" pixelGetYellowQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetYellowQuantum() sets the blue color of the pixel wand.
foreign import ccall "PixelSetYellowQuantum" pixelSetYellowQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

-- | PixelGetBlack) returns the normalized blue color of the pixel wand.
foreign import ccall "PixelGetBlack" pixelGetBlack
  :: Ptr PixelWand -> IO CDouble

foreign import ccall "PixelSetBlack" pixelSetBlack
  :: Ptr PixelWand -> CDouble -> IO ()

-- | PixelGetBlackQuantum() returns the blue color of the pixel wand.
foreign import ccall "PixelGetBlackQuantum" pixelGetBlackQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetBlackQuantum() sets the blue color of the pixel wand.
foreign import ccall "PixelSetBlackQuantum" pixelSetBlackQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

-- | PixelGetAlpha) returns the normalized blue color of the pixel wand.
foreign import ccall "PixelGetAlpha" pixelGetAlpha
  :: Ptr PixelWand -> IO CDouble

-- | PixelGetAlphaQuantum() returns the blue color of the pixel wand.
foreign import ccall "PixelGetAlphaQuantum" pixelGetAlphaQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetAlphaQuantum() sets the blue color of the pixel wand.
foreign import ccall "PixelSetAlphaQuantum" pixelSetAlphaQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

foreign import ccall "PixelSetAlpha" pixelSetAlpha
  :: Ptr PixelWand -> CDouble -> IO ()

-- | PixelGetOpacity) returns the normalized blue color of the pixel wand.
foreign import ccall "PixelGetOpacity" pixelGetOpacity
  :: Ptr PixelWand -> IO CDouble

-- | PixelGetOpacityQuantum() returns the blue color of the pixel wand.
foreign import ccall "PixelGetOpacityQuantum" pixelGetOpacityQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetOpacityQuantum() sets the blue color of the pixel wand.
foreign import ccall "PixelSetOpacityQuantum" pixelSetOpacityQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

foreign import ccall "PixelSetOpacity" pixelSetOpacity
  :: Ptr PixelWand -> CDouble -> IO ()



-- | PixelGetColorCount() returns the color count associated with this color.
foreign import ccall "PixelGetColorCount" pixelGetColorCount
  :: Ptr PixelWand -> IO CSize

-- | PixelSetColorCount() sets the color count of the pixel wand.
foreign import ccall "PixelSetColorCount" pixelSetColorCount
  :: Ptr PixelWand -> CSize -> IO ()

-- PixelGetHSL() returns the normalized HSL color of the pixel wand.
foreign import ccall "PixelGetHSL" pixelGetHSL
  :: Ptr PixelWand -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

-- | PixelSetHSL() sets the normalized HSL color of the pixel wand.
foreign import ccall "PixelSetHSL" pixelSetHSL
  :: Ptr PixelWand -> CDouble -> CDouble -> CDouble -> IO ()


-- | PixelSetColorFromWand() sets the color of the pixel wand.
foreign import ccall "PixelSetColorFromWand" pixelSetColorFromWand
  :: Ptr PixelWand -> Ptr PixelWand -> IO ()


-- | PixelGetIndex() returns the colormap index from the pixel wand.
foreign import ccall "PixelGetIndex" pixelGetIndex
  :: Ptr PixelWand -> IO IndexPacket


-- | PixelSetIndex() sets the colormap index of the pixel wand.
foreign import ccall "PixelSetColor" pixelSetIndex
  :: Ptr PixelWand -> IndexPacket -> IO ()

-- | PixelGetQuantumColor() gets the color of the pixel wand as a PixelPacket.
foreign import ccall "PixelGetQuantumColor" pixelGetQuantumColor
  :: Ptr PixelWand -> Ptr PixelPacket -> IO ()

-- | PixelGetQuantumColor() gets the color of the pixel wand as a PixelPacket.
foreign import ccall "PixelSetQuantumColor" pixelSetQuantumColor
  :: Ptr PixelWand -> Ptr PixelPacket -> IO ()

-- | PixelGetFuzz() returns the normalized fuzz value of the pixel wand.
foreign import ccall "PixelGetFuzz" pixelGetFuzz
  :: Ptr PixelWand -> IO CDouble

-- | PixelSetFuzz() sets the fuzz value of the pixel wand.
foreign import ccall "PixelSetFuzz" pixelSetFuzz
  :: Ptr PixelWand -> CDouble -> IO ()

{-
ClonePixelWands() makes an exact copy of the specified wands.

The format of the ClonePixelWands method is:

  PixelWand **ClonePixelWands(const PixelWand **wands,
    const size_t number_wands)

-}
