{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickWand.FFI.PixelIterator
  where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.Types

#include <wand/MagickWand.h>

-- | ClearPixelIterator() clear resources associated with a PixelIterator.
foreign import ccall "wand/MagickWand.h ClearPixelIterator" clearPixelIterator
  :: Ptr PixelIterator -> IO ()

-- | ClonePixelIterator() makes an exact copy of the specified iterator.
foreign import ccall "wand/MagickWand.h ClonePixelIterator" clonePixelIterator
  :: Ptr PixelIterator -> IO (Ptr PixelIterator)

-- | DestroyPixelIterator() deallocates resources associated with a PixelIterator.
foreign import ccall "wand/MagickWand.h DestroyPixelIterator" destroyPixelIterator
  :: Ptr PixelIterator -> IO (Ptr PixelIterator)

-- | IsPixelIterator() returns MagickTrue if the iterator is verified as a pixel iterator.
foreign import ccall "wand/MagickWand.h IsPixelIterator" isPixelIterator
  :: Ptr PixelIterator -> IO MagickBooleanType

foreign import ccall "wand/MagickWand.h NewPixelIterator" newPixelIterator
  :: Ptr MagickWand -> IO (Ptr PixelIterator)


-- | PixelClearIteratorException() clear any exceptions associated with the iterator.
foreign import ccall "wand/MagickWand.h PixelClearIteratorException" pixelClearIteratorException
  :: Ptr PixelIterator -> IO MagickBooleanType

-- | PixelGetIteratorException() returns the severity, reason, and description of any
-- error that occurs when using other methods in this API.
foreign import ccall "PixelGetIteratorException" pixelGetIteratorException
  :: Ptr PixelIterator -> Ptr ExceptionType -> IO CString

-- | PixelGetIteratorExceptionType() the exception type associated with the iterator.
--   If no exception has occurred, UndefinedExceptionType is returned.
foreign import ccall "PixelGetIteratorExceptionType" pixelGetIteratorExceptionType
  :: Ptr PixelIterator -> IO ExceptionType


-- | NewPixelRegionIterator() returns a new pixel iterator.
foreign import ccall "wand/MagickWand.h NewPixelRegionIterator" newPixelRegionIterator
  :: Ptr MagickWand
  -> CSize                        -- ^ x        top X
  -> CSize                        -- ^ y        top Y
  -> CSize                        -- ^ width
  -> CSize                        -- ^ Height
  -> IO (Ptr PixelIterator)

{- | PixelGetCurrentIteratorRow() returns the current row as an array of pixel wands from the pixel iterator.
-}
foreign import ccall "wand/MagickWand.h PixelGetCurrentIteratorRow" pixelGetCurrentIteratorRow
  :: Ptr PixelIterator -> CSize -> Ptr (Ptr PixelWand)

-- | PixelGetIteratorRow() returns the current pixel iterator row.
foreign import ccall "wand/MagickWand.h PixelGetIteratorRow" pixelGetIteratorRow
  :: Ptr PixelIterator -> IO ()

-- | PixelGetNextIteratorRow() returns the next row as an array of pixel wands from the pixel iterator.
foreign import ccall "wand/MagickWand.h PixelGetNextIteratorRow" pixelGetNextIteratorRow
  :: Ptr PixelIterator  -- ^ iterator
  -> Ptr CSize          -- ^ number of pixel wands
  -> IO (Ptr (Ptr PixelWand))

{-
      PixelGetPreviousIteratorRow

      PixelGetPreviousIteratorRow() returns the previous row as an array of pixel wands from the pixel iterator.

      The format of the PixelGetPreviousIteratorRow method is:

        PixelWand **PixelGetPreviousIteratorRow(PixelIterator *iterator,
            size_t *number_wands)

            A description of each parameter follows:
            iterator

            the pixel iterator.
            number_wands

            the number of pixel wands.
            PixelResetIterator

            PixelResetIterator() resets the pixel iterator. Use it in conjunction with PixelGetNextIteratorRow() to iterate over all the pixels in a pixel container.

            The format of the PixelResetIterator method is:

              void PixelResetIterator(PixelIterator *iterator)

              A description of each parameter follows:
              iterator

              the pixel iterator.
              PixelSetFirstIteratorRow

              PixelSetFirstIteratorRow() sets the pixel iterator to the first pixel row.

              The format of the PixelSetFirstIteratorRow method is:

                void PixelSetFirstIteratorRow(PixelIterator *iterator)

                A description of each parameter follows:
                iterator

                the magick iterator.
                PixelSetIteratorRow

                PixelSetIteratorRow() set the pixel iterator row.

                The format of the PixelSetIteratorRow method is:

                  MagickBooleanType PixelSetIteratorRow(PixelIterator *iterator,
                      const ssize_t row)

                      A description of each parameter follows:
                      iterator

                      the pixel iterator.
                      PixelSetLastIteratorRow

                      PixelSetLastIteratorRow() sets the pixel iterator to the last pixel row.

                      The format of the PixelSetLastIteratorRow method is:

                        void PixelSetLastIteratorRow(PixelIterator *iterator)

                        A description of each parameter follows:
                        iterator

                        the magick iterator.
-}

foreign import ccall "PixelSyncIterator" pixelSyncIterator
  :: Ptr PixelIterator -> IO (MagickBooleanType)
