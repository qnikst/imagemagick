{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickWand.FFI.MagickWand
  where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.Types

#include <wand/MagickWand.h>


-- | MagickWandGenesis() initializes the MagickWand environment.
foreign import ccall "MagickWandGenesis" magickWandGenesis
  :: IO ()

-- | MagickWandTerminus() terminates the MagickWand environment.
foreign import ccall "MagickWandTerminus" magickWandTerminus
  :: IO ()

-- * Constructing magic wand

-- | NewMagickWand() returns a wand required for all other methods in the API.
-- A fatal exception is thrown if there is not enough memory to allocate the wand.
-- Use DestroyMagickWand() to dispose of the wand when it is no longer needed.
foreign import ccall "NewMagickWand" newMagickWand
  :: IO (Ptr MagickWand)


-- | NewMagickWandFromImage() returns a wand with an image.
foreign import ccall "NewMagickWandFromImage" newMagickWandFromImage
  :: Ptr Image                  -- ^ Image
  -> IO (Ptr MagickWand)

-- | CloneMagickWand() makes an exact copy of the specified wand.
foreign import ccall "CloneMagickWand" cloneMagickWand
  :: Ptr MagickWand -> IO (Ptr MagickWand)

-- * Clearing and destroying

-- | ClearMagickWand() clears resources associated with the wand,
-- leaving the wand blank, and ready to be used for a new set of images.
foreign import ccall "ClearMagickWand" clearMagickWand
  :: Ptr MagickWand -> IO ()

-- | DestroyMagickWand() deallocates memory associated with an MagickWand.
foreign import ccall "DestroyMagickWand" destroyMagickWand
  :: Ptr MagickWand -> IO (Ptr MagickWand)

foreign import ccall "&DestroyMagickWand" pDestroyMagickWand
  :: FunPtr (Ptr MagickWand -> IO ())
-- * Utilities

-- | IsMagickWand() returns MagickTrue if the wand is verified as a magick wand.
foreign import ccall " IsMagickWand" isMagicWand
  :: Ptr MagickWand -> IO MagickBooleanType

-- * Exceptions

-- | MagickClearException() clears any exceptions associated with the wand.
--
foreign import ccall "MagickClearException" magickClearException
  :: Ptr MagickWand -> IO MagickBooleanType

-- | MagickGetException() returns the severity, reason, and description of
--   any error that occurs when using other methods in this API.
foreign import ccall "MagickGetException" magickGetException
  :: Ptr MagickWand -> Ptr ExceptionType -> IO CString

-- | MagickGetExceptionType() returns the exception type associated with the wand.
--   If no exception has occurred, UndefinedExceptionType is returned.
foreign import ccall "MagickGetExceptionType" magickGetExceptionType
  :: Ptr MagickWand -> IO ExceptionType


-- | MagickGetIteratorIndex() returns the position of the iterator in the image list.
foreign import ccall "MagickGetIteratorIndex" magickGetIteratorIndex :: Ptr MagickWand -> IO CSize


{-

MagickQueryConfigureOption() returns the value associated with the specified configure option.
  char *MagickQueryConfigureOption(const char *option)

MagickQueryConfigureOptions() returns any configure options that match the specified pattern (e.g. "*" for all). Options include NAME, VERSION, LIB_VERSION, etc.

The format of the MagickQueryConfigureOptions function is:

  char **MagickQueryConfigureOptions(const char *pattern,
    size_t *number_options)

A description of each parameter follows:
pattern

Specifies a pointer to a text string containing a pattern.
number_options

Returns the number of configure options in the list.



MagickQueryFontMetrics() returns a 13 element array representing the following font metrics:

      Element Description
      -------------------------------------------------
      0 character width
      1 character height
      2 ascender
      3 descender
      4 text width
      5 text height
      6 maximum horizontal advance
      7 bounding box: x1
      8 bounding box: y1
      9 bounding box: x2
     10 bounding box: y2
     11 origin: x
     12 origin: y

The format of the MagickQueryFontMetrics method is:

  double *MagickQueryFontMetrics(MagickWand *wand,
    const DrawingWand *drawing_wand,const char *text)

A description of each parameter follows:
wand

the Magick wand.
drawing_wand

the drawing wand.
text

the text.


MagickQueryMultilineFontMetrics() returns a 13 element array representing the following font metrics:

      Element Description
      -------------------------------------------------
      0 character width
      1 character height
      2 ascender
      3 descender
      4 text width
      5 text height
      6 maximum horizontal advance
      7 bounding box: x1
      8 bounding box: y1
      9 bounding box: x2
     10 bounding box: y2
     11 origin: x
     12 origin: y

This method is like MagickQueryFontMetrics() but it returns the maximum text width and height for multiple lines of text.

The format of the MagickQueryFontMetrics method is:

  double *MagickQueryMultilineFontMetrics(MagickWand *wand,
    const DrawingWand *drawing_wand,const char *text)

A description of each parameter follows:
wand

the Magick wand.
drawing_wand

the drawing wand.
text

the text.



MagickQueryFonts() returns any font that match the specified pattern (e.g. "*" for all).

The format of the MagickQueryFonts function is:

  char **MagickQueryFonts(const char *pattern,size_t *number_fonts)

A description of each parameter follows:
pattern

Specifies a pointer to a text string containing a pattern.
number_fonts

Returns the number of fonts in the list.



MagickQueryFonts() returns any image formats that match the specified pattern (e.g. "*" for all).

The format of the MagickQueryFonts function is:

  char **MagickQueryFonts(const char *pattern,
    size_t *number_formats)

A description of each parameter follows:
pattern

Specifies a pointer to a text string containing a pattern.
number_formats

This integer returns the number of image formats in the list.








MagickSetFirstIterator() sets the wand iterator to the first image.

After using any images added to the wand using MagickAddImage() or MagickReadImage() will be prepended before any image in the wand.

Also the current image has been set to the first image (if any) in the Magick Wand. Using MagickNextImage() will then set teh current image to the second image in the list (if present).

This operation is similar to MagickResetIterator() but differs in how MagickAddImage(), MagickReadImage(), and MagickNextImage() behaves afterward.

The format of the MagickSetFirstIterator method is:

  void MagickSetFirstIterator(MagickWand *wand)

A description of each parameter follows:
wand

the magick wand.


MagickSetIteratorIndex() set the iterator to the given position in the image list specified with the index parameter. A zero index will set the first image as current, and so on. Negative indexes can be used to specify an image relative to the end of the images in the wand, with -1 being the last image in the wand.

If the index is invalid (range too large for number of images in wand) the function will return MagickFalse, but no 'exception' will be raised, as it is not actually an error. In that case the current image will not change.

After using any images added to the wand using MagickAddImage() or MagickReadImage() will be added after the image indexed, regardless of if a zero (first image in list) or negative index (from end) is used.

Jumping to index 0 is similar to MagickResetIterator() but differs in how MagickNextImage() behaves afterward.

The format of the MagickSetIteratorIndex method is:

  MagickBooleanType MagickSetIteratorIndex(MagickWand *wand,
    const ssize_t index)

A description of each parameter follows:
wand

the magick wand.
index

the scene number.



MagickSetLastIterator() sets the wand iterator to the last image.

The last image is actually the current image, and the next use of MagickPreviousImage() will not change this allowing this function to be used to iterate over the images in the reverse direction. In this sense it is more like MagickResetIterator() than MagickSetFirstIterator().

Typically this function is used before MagickAddImage(), MagickReadImage() functions to ensure new images are appended to the very end of wand's image list.

The format of the MagickSetLastIterator method is:

  void MagickSetLastIterator(MagickWand *wand)

A description of each parameter follows:
wand

the magick wand.

-}

-- * Image functions

-- | MagickReadImage() reads an image or image sequence. The images are inserted at
-- the current image pointer position. Use MagickSetFirstIterator(), MagickSetLastIterator,
-- or MagickSetImageIndex() to specify the current image pointer position at the beginning
-- of the image list, the end, or anywhere in-between respectively.

foreign import ccall "MagickReadImage" magickReadImage
  :: Ptr MagickWand -> CString -> IO MagickBooleanType

-- | MagickNextImage() sets the next image in the wand as the current image.
-- It is typically used after MagickResetIterator(), after which its first use will
-- set the first image as the current image (unless the wand is empty).
--
-- It will return MagickFalse when no more images are left to be returned which
-- happens when the wand is empty, or the current image is the last image.
--
-- When the above condition (end of image list) is reached, the iterator is
-- automatically set so that you can start using MagickPreviousImage() to again
-- iterate over the images in the reverse direction, starting with the last image
-- (again). You can jump to this condition immeditally using MagickSetLastIterator().
foreign import ccall "MagickNextImage" magickNextImage
  :: Ptr MagickWand -> IO MagickBooleanType


-- | MagickPreviousImage() sets the previous image in the wand as the current image.
--
-- It is typically used after MagickSetLastIterator(), after which its first use
-- will set the last image as the current image (unless the wand is empty).
--
-- It will return MagickFalse when no more images are left to be returned which
-- happens when the wand is empty, or the current image is the first image.
-- At that point the iterator is than reset to again process images in the forward
-- direction, again starting with the first image in list. Images added at this
-- point are prepended.
--
-- Also at that point any images added to the wand using MagickAddImages() or
-- MagickReadImages() will be prepended before the first image. In this sense the
-- condition is not quite exactly the same as MagickResetIterator().
foreign import ccall "MagickPreviousImage" magickPreviousImage
  :: Ptr MagickWand -> IO MagickBooleanType

{-
  ResizeFilter      Bessel   Blackman   Box
  Catrom   CubicGaussian
  Hanning  Hermite    Lanczos
  Mitchell PointQuandratic
  Sinc     Triangle
-}

-- | MagickResizeImage() scales an image to the desired dimensions with one of these filters:
--
-- Most of the filters are FIR (finite impulse response), however, Bessel, Gaussian, and Sinc are
-- IIR (infinite impulse response). Bessel and Sinc are windowed (brought down to zero) with the Blackman filter.

foreign import ccall "MagickResizeImage" magickResizeImage
  :: Ptr MagickWand
  -> CSize                        -- ^ the number of columns in the scaled image
  -> CSize                        -- ^ the number of rows in the scaled image
  -> FilterTypes                  -- ^ filter
  -> CDouble                      -- ^ blur factor where 1 > blurry, < 1 sharp
  -> IO MagickBooleanType


-- | MagickWriteImages() writes an image or image sequence.
foreign import ccall "MagickWriteImages" magickWriteImages
  :: Ptr MagickWand
  -> CString                      -- ^ filename
  -> MagickBooleanType            -- ^ join images into a single multi-image file.
  -> IO MagickBooleanType


-- |  MagickSetSize() sets the size of the magick wand. Set it before you read a raw image format such as RGB, GRAY, or CMYK.
foreign import ccall "MagickSetSize" magickSetSize
  :: Ptr MagickWand
  -> CSize                        -- ^ columns
  -> CSize                        -- ^ rows
  -> IO MagickBooleanType


-- | MagickGetSize() returns the size associated with the magick wand.
foreign import ccall "MagickGetSize" magickGetSize
  :: Ptr MagickWand
  -> Ptr CSize                    -- ^ the width in pixels
  -> Ptr CSize                    -- ^ the height in pixels
  -> IO MagickBooleanType


-- |  MagickGaussianBlurImage() blurs an image. We convolve the image with a Gaussian operator
-- of the given radius and standard deviation (sigma). For reasonable results, the radius should
-- be larger than sigma. Use a radius of 0 and MagickGaussianBlurImage() selects a suitable radius for you.
foreign import ccall "MagickGaussianBlurImage" magickGaussianBlurImage
  :: Ptr MagickWand
  -> CDouble    -- ^ radius
  -> CDouble    -- ^ sigma
  -> IO MagickBooleanType


foreign import ccall "MagickGaussianBlurImage" magickGaussianBlurImageChannel
  :: Ptr MagickWand
  -> ChannelType
  -> CDouble    -- ^ radius
  -> CDouble    -- ^ sigma
  -> IO MagickBooleanType


-- | MagickSetImageArtifact() associates a artifact with an image.
-- The format of the MagickSetImageArtifact method is:
foreign import ccall "MagickSetImageArtifact" magickSetImageArtifact
  :: Ptr MagickWand
  -> CString
  -> CString
  -> IO MagickBooleanType

-- | MagickDeleteImageArtifact() deletes a wand artifact.
foreign import ccall "MagickDeleteImageArtifact" magickDeleteImageArtifact
  :: Ptr MagickWand
  -> CString
  -> IO MagickBooleanType

-- | MagickSetIteratorIndex() set the iterator to the given position
-- in the image list specified with the index parameter. A zero index
-- will set the first image as current, and so on. Negative indexes
-- can be used to specify an image relative to the end of the images
-- in the wand, with -1 being the last image in the wand.
--
-- If the index is invalid (range too large for number of images in
-- wand) the function will return MagickFalse, but no 'exception' will
-- be raised, as it is not actually an error. In that case the current
-- image will not change.
--
-- After using any images added to the wand using `magickAddImage` or
-- `magickReadImage` will be added after the image indexed, regardless
-- of if a zero (first image in list) or negative index (from end)
-- is used.
--
-- Jumping to index 0 is similar to `magickResetIterator` but differs
-- in how `magickNextImage` behaves afterward.
foreign import ccall "MagickSetIteratorIndex" magickSetIteratorIndex
  :: Ptr MagickWand
  -> CSize          -- ^ the scene number
  -> IO MagickBooleanType

-- | MagickResetIterator() resets the wand iterator.
--
-- It is typically used either before iterating though images, or
-- before calling specific functions such as `magickAppendImages`
-- to append all images together.
--
-- Afterward you can use `magickNextImage` to iterate over all the
-- images in a wand container, starting with the first image.
--
-- Using this before `magickAddImages` or `magickReadImages` will
-- cause new images to be inserted between the first and second image.
foreign import ccall "MagickResetIterator" magickResetIterator
  :: Ptr MagickWand
  -> IO ()

-- | MagickSetLastIterator() sets the wand iterator to the last image.
-- The last image is actually the current image, and the next use of
-- MagickPreviousImage() will not change this allowing this function
-- to be used to iterate over the images in the reverse direction.
-- In this sense it is more like MagickResetIterator() than
-- MagickSetFirstIterator().
-- Typically this function is used before MagickAddImage(),
-- MagickReadImage() functions to ensure new images are appended to
-- the very end of wand's image list.
foreign import ccall "MagickSetFirstIterator" magickSetFirstIterator
  :: Ptr MagickWand
  -> IO ()

-- | MagickSetFirstIterator() sets the wand iterator to the first image.
-- After using any images added to the wand using MagickAddImage() or
-- MagickReadImage() will be prepended before any image in the wand.
-- Also the current image has been set to the first image (if any) in
-- the Magick Wand. Using MagickNextImage() will then set teh current
-- image to the second image in the list (if present).
-- This operation is similar to MagickResetIterator() but differs in
-- how MagickAddImage(), MagickReadImage(), and MagickNextImage()
-- behaves afterward.
foreign import ccall "MagickSetLastIterator" magickSetLastIterator
  :: Ptr MagickWand
  -> IO ()

-- | MagickRelinquishMemory() relinquishes memory resources returned
-- by such methods as MagickIdentifyImage(), MagickGetException(), etc.
foreign import ccall "MagickRelinquishMemory" magickRelinquishMemory
  :: Ptr () -> IO ()
