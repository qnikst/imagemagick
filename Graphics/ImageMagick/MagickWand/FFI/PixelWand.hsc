{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickWand.FFI.PixelWand
  where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.Types
import           Graphics.ImageMagick.MagickCore.Types.FFI.Types

#include <wand/MagickWand.h>

-- | DestroyPixelWand() deallocates resources associated with a PixelWand.

foreign import ccall "DestroyPixelWand" destroyPixelWand
  :: Ptr PixelWand -> IO (Ptr PixelWand)


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

-- | PixelSetRedQuantum() sets the red color of the pixel wand.
foreign import ccall "PixelSetRedQuantum" pixelSetRedQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

-- | PixelGetRedQuantum() returns the red color of the pixel wand.
foreign import ccall "PixelGetRedQuantum" pixelGetRedQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelGetGreenQuantum() returns the green color of the pixel wand.
foreign import ccall "PixelGetGreenQuantum" pixelGetGreenQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetGreenQuantum() sets the green color of the pixel wand.
foreign import ccall "PixelSetGreenQuantum" pixelSetGreenQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

-- | PixelGetBlueQuantum() returns the blue color of the pixel wand.
foreign import ccall "PixelGetBlueQuantum" pixelGetBlueQuantum
  :: Ptr PixelWand -> IO Quantum

-- | PixelSetBlueQuantum() sets the blue color of the pixel wand.
foreign import ccall "PixelSetBlueQuantum" pixelSetBlueQuantum
  :: Ptr PixelWand -> Quantum -> IO ()

{-
ClonePixelWands() makes an exact copy of the specified wands.

The format of the ClonePixelWands method is:

  PixelWand **ClonePixelWands(const PixelWand **wands,
    const size_t number_wands)

A description of each parameter follows:
wands

the magick wands.
number_wands

the number of wands.

DestroyPixelWands

DestroyPixelWands() deallocates resources associated with an array of pixel wands.

The format of the DestroyPixelWands method is:

  PixelWand **DestroyPixelWands(PixelWand **wand,
    const size_t number_wands)

A description of each parameter follows:
wand

the pixel wand.
number_wands

the number of wands.
IsPixelWandSimilar

IsPixelWandSimilar() returns MagickTrue if the distance between two colors is less than the specified distance.

The format of the IsPixelWandSimilar method is:

  MagickBooleanType IsPixelWandSimilar(PixelWand *p,PixelWand *q,
    const double fuzz)

A description of each parameter follows:
p

the pixel wand.
q

the pixel wand.
fuzz

any two colors that are less than or equal to this distance squared are consider similar.
IsPixelWand

IsPixelWand() returns MagickTrue if the wand is verified as a pixel wand.

The format of the IsPixelWand method is:

  MagickBooleanType IsPixelWand(const PixelWand *wand)

A description of each parameter follows:
wand

the magick wand.

A description of each parameter follows:
wand

the pixel wand.
PixelGetAlpha

PixelGetAlpha() returns the normalized alpha color of the pixel wand.

The format of the PixelGetAlpha method is:

  double PixelGetAlpha(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetAlphaQuantum

PixelGetAlphaQuantum() returns the alpha value of the pixel wand.

The format of the PixelGetAlphaQuantum method is:

  Quantum PixelGetAlphaQuantum(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetBlack

PixelGetBlack() returns the normalized black color of the pixel wand.

The format of the PixelGetBlack method is:

  double PixelGetBlack(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetBlackQuantum

PixelGetBlackQuantum() returns the black color of the pixel wand.

The format of the PixelGetBlackQuantum method is:

  Quantum PixelGetBlackQuantum(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetBlue

PixelGetBlue() returns the normalized blue color of the pixel wand.

The format of the PixelGetBlue method is:

  double PixelGetBlue(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetColorAsString

PixelGetColorAsString() returnsd the color of the pixel wand as a string.

The format of the PixelGetColorAsString method is:

  char *PixelGetColorAsString(PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetColorAsNormalizedString

PixelGetColorAsNormalizedString() returns the normalized color of the pixel wand as a string.

The format of the PixelGetColorAsNormalizedString method is:

  char *PixelGetColorAsNormalizedString(PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetColorCount

PixelGetColorCount() returns the color count associated with this color.

The format of the PixelGetColorCount method is:

  size_t PixelGetColorCount(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetCyan

PixelGetCyan() returns the normalized cyan color of the pixel wand.

The format of the PixelGetCyan method is:

  double PixelGetCyan(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetCyanQuantum

PixelGetCyanQuantum() returns the cyan color of the pixel wand.

The format of the PixelGetCyanQuantum method is:

  Quantum PixelGetCyanQuantum(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetFuzz

PixelGetFuzz() returns the normalized fuzz value of the pixel wand.

The format of the PixelGetFuzz method is:

  double PixelGetFuzz(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetGreen

PixelGetGreen() returns the normalized green color of the pixel wand.

The format of the PixelGetGreen method is:

  double PixelGetGreen(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.

the pixel wand.
PixelGetHSL

PixelGetHSL() returns the normalized HSL color of the pixel wand.

The format of the PixelGetHSL method is:

  void PixelGetHSL(const PixelWand *wand,double *hue,double *saturation,
    double *lightness)

A description of each parameter follows:
wand

the pixel wand.
hue,saturation,lightness

Return the pixel hue, saturation, and brightness.
PixelGetIndex

PixelGetIndex() returns the colormap index from the pixel wand.

The format of the PixelGetIndex method is:

  IndexPacket PixelGetIndex(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetMagenta

PixelGetMagenta() returns the normalized magenta color of the pixel wand.

The format of the PixelGetMagenta method is:

  double PixelGetMagenta(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetMagentaQuantum

PixelGetMagentaQuantum() returns the magenta color of the pixel wand.

The format of the PixelGetMagentaQuantum method is:

  Quantum PixelGetMagentaQuantum(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetMagickColor

PixelGetMagickColor() gets the magick color of the pixel wand.

The format of the PixelGetMagickColor method is:

  void PixelGetMagickColor(PixelWand *wand,MagickPixelPacket *color)

A description of each parameter follows:
wand

the pixel wand.
color

The pixel wand color is returned here.
PixelGetOpacity

PixelGetOpacity() returns the normalized opacity color of the pixel wand.

The format of the PixelGetOpacity method is:

  double PixelGetOpacity(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetOpacityQuantum

PixelGetOpacityQuantum() returns the opacity color of the pixel wand.

The format of the PixelGetOpacityQuantum method is:

  Quantum PixelGetOpacityQuantum(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetQuantumColor

PixelGetQuantumColor() gets the color of the pixel wand as a PixelPacket.

The format of the PixelGetQuantumColor method is:

  void PixelGetQuantumColor(PixelWand *wand,PixelPacket *color)

A description of each parameter follows:
wand

the pixel wand.
color

The pixel wand color is returned here.
PixelGetRed

PixelGetRed() returns the normalized red color of the pixel wand.

The format of the PixelGetRed method is:

  double PixelGetRed(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.

A description of each parameter follows:
wand

the pixel wand.
PixelGetYellow

PixelGetYellow() returns the normalized yellow color of the pixel wand.

The format of the PixelGetYellow method is:

  double PixelGetYellow(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelGetYellowQuantum

PixelGetYellowQuantum() returns the yellow color of the pixel wand.

The format of the PixelGetYellowQuantum method is:

  Quantum PixelGetYellowQuantum(const PixelWand *wand)

A description of each parameter follows:
wand

the pixel wand.
PixelSetAlpha

PixelSetAlpha() sets the normalized alpha color of the pixel wand.

The format of the PixelSetAlpha method is:

  void PixelSetAlpha(PixelWand *wand,const double alpha)

A description of each parameter follows:
wand

the pixel wand.
alpha

the level of transparency: 1.0 is fully opaque and 0.0 is fully transparent.
PixelSetAlphaQuantum

PixelSetAlphaQuantum() sets the alpha color of the pixel wand.

The format of the PixelSetAlphaQuantum method is:

  void PixelSetAlphaQuantum(PixelWand *wand,
    const Quantum opacity)

A description of each parameter follows:
wand

the pixel wand.
opacity

the opacity color.
PixelSetBlack

PixelSetBlack() sets the normalized black color of the pixel wand.

The format of the PixelSetBlack method is:

  void PixelSetBlack(PixelWand *wand,const double black)

A description of each parameter follows:
wand

the pixel wand.
black

the black color.
PixelSetBlackQuantum

PixelSetBlackQuantum() sets the black color of the pixel wand.

The format of the PixelSetBlackQuantum method is:

  void PixelSetBlackQuantum(PixelWand *wand,const Quantum black)

A description of each parameter follows:
wand

the pixel wand.
black

the black color.
PixelSetBlue

PixelSetBlue() sets the normalized blue color of the pixel wand.

The format of the PixelSetBlue method is:

  void PixelSetBlue(PixelWand *wand,const double blue)

A description of each parameter follows:
wand

the pixel wand.
blue

the blue color.
PixelSetColorCount

PixelSetColorCount() sets the color count of the pixel wand.

The format of the PixelSetColorCount method is:

  void PixelSetColorCount(PixelWand *wand,const size_t count)

A description of each parameter follows:
wand

the pixel wand.
count

the number of this particular color.
PixelSetColorFromWand

PixelSetColorFromWand() sets the color of the pixel wand.

The format of the PixelSetColorFromWand method is:

  void PixelSetColorFromWand(PixelWand *wand,const PixelWand *color)

A description of each parameter follows:
wand

the pixel wand.
color

set the pixel wand color here.
PixelSetCyan

PixelSetCyan() sets the normalized cyan color of the pixel wand.

The format of the PixelSetCyan method is:

  void PixelSetCyan(PixelWand *wand,const double cyan)

A description of each parameter follows:
wand

the pixel wand.
cyan

the cyan color.
PixelSetCyanQuantum

PixelSetCyanQuantum() sets the cyan color of the pixel wand.

The format of the PixelSetCyanQuantum method is:

  void PixelSetCyanQuantum(PixelWand *wand,const Quantum cyan)

A description of each parameter follows:
wand

the pixel wand.
cyan

the cyan color.
PixelSetFuzz

PixelSetFuzz() sets the fuzz value of the pixel wand.

The format of the PixelSetFuzz method is:

  void PixelSetFuzz(PixelWand *wand,const double fuzz)

A description of each parameter follows:
wand

the pixel wand.
fuzz

the fuzz value.
PixelSetGreen

PixelSetGreen() sets the normalized green color of the pixel wand.

The format of the PixelSetGreen method is:

  void PixelSetGreen(PixelWand *wand,const double green)

A description of each parameter follows:
wand

the pixel wand.
green

the green color.
PixelSetHSL

PixelSetHSL() sets the normalized HSL color of the pixel wand.

The format of the PixelSetHSL method is:

  void PixelSetHSL(PixelWand *wand,const double hue,
    const double saturation,const double lightness)

A description of each parameter follows:
wand

the pixel wand.
hue,saturation,lightness

Return the pixel hue, saturation, and brightness.
PixelSetIndex

PixelSetIndex() sets the colormap index of the pixel wand.

The format of the PixelSetIndex method is:

  void PixelSetIndex(PixelWand *wand,const IndexPacket index)

A description of each parameter follows:
wand

the pixel wand.
index

the colormap index.
PixelSetMagenta

PixelSetMagenta() sets the normalized magenta color of the pixel wand.

The format of the PixelSetMagenta method is:

  void PixelSetMagenta(PixelWand *wand,const double magenta)

A description of each parameter follows:
wand

the pixel wand.
magenta

the magenta color.
PixelSetMagentaQuantum

PixelSetMagentaQuantum() sets the magenta color of the pixel wand.

The format of the PixelSetMagentaQuantum method is:

  void PixelSetMagentaQuantum(PixelWand *wand,
    const Quantum magenta)

A description of each parameter follows:
wand

the pixel wand.
magenta

the green magenta.
PixelSetMagickColor

PixelSetMagickColor() sets the color of the pixel wand.

The format of the PixelSetMagickColor method is:

  void PixelSetMagickColor(PixelWand *wand,const MagickPixelPacket *color)

A description of each parameter follows:
wand

the pixel wand.
color

the pixel wand color.
PixelSetOpacity

PixelSetOpacity() sets the normalized opacity color of the pixel wand.

The format of the PixelSetOpacity method is:

  void PixelSetOpacity(PixelWand *wand,const double opacity)

A description of each parameter follows:
wand

the pixel wand.
opacity

the opacity color.
PixelSetOpacityQuantum

PixelSetOpacityQuantum() sets the opacity color of the pixel wand.

The format of the PixelSetOpacityQuantum method is:

  void PixelSetOpacityQuantum(PixelWand *wand,
    const Quantum opacity)

A description of each parameter follows:
wand

the pixel wand.
opacity

the opacity color.
PixelSetQuantumColor

PixelSetQuantumColor() sets the color of the pixel wand.

The format of the PixelSetQuantumColor method is:

  void PixelSetQuantumColor(PixelWand *wand,const PixelPacket *color)

A description of each parameter follows:
wand

the pixel wand.
color

the pixel wand color.
PixelSetRed

PixelSetRed() sets the normalized red color of the pixel wand.

The format of the PixelSetRed method is:

  void PixelSetRed(PixelWand *wand,const double red)

A description of each parameter follows:
wand

the pixel wand.
red

the red color.
PixelSetYellow

PixelSetYellow() sets the normalized yellow color of the pixel wand.

The format of the PixelSetYellow method is:

  void PixelSetYellow(PixelWand *wand,const double yellow)

A description of each parameter follows:
wand

the pixel wand.
yellow

the yellow color.
PixelSetYellowQuantum

PixelSetYellowQuantum() sets the yellow color of the pixel wand.

The format of the PixelSetYellowQuantum method is:

  void PixelSetYellowQuantum(PixelWand *wand,const Quantum yellow)

A description of each parameter follows:
wand

the pixel wand.
yellow

the yellow color.
-}
