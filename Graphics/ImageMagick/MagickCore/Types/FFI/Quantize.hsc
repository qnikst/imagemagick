{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.ImageMagick.MagickCore.Types.FFI.Quantize where

import           Graphics.ImageMagick.MagickCore.Types (ColorspaceType, DitherMethod)
import           Graphics.ImageMagick.MagickWand.FFI.Types (MagickBooleanType)
import           Foreign.C.Types (CSize)
import           Foreign.Storable

#include <magick/MagickCore.h>

data QuantizeInfo = QuantizeInfo { numberOfColors :: CSize
                                 , treeDepth :: CSize
                                 , shouldDither :: MagickBooleanType
                                 , colorspace :: ColorspaceType
                                 , measureError :: MagickBooleanType
                                 , signature :: CSize
                                 , ditherMethod :: DitherMethod
                                 }
  deriving (Eq, Show)

instance Storable QuantizeInfo where
  sizeOf      _ = (#size QuantizeInfo)
  alignment   _ = alignment (undefined :: CSize)
  peek ptr      = do
    numberOfColors' <- (#peek QuantizeInfo, number_colors) ptr
    treeDepth'      <- (#peek QuantizeInfo, tree_depth) ptr
    shouldDither'   <- (#peek QuantizeInfo, dither) ptr
    colorspace'     <- (#peek QuantizeInfo, colorspace) ptr
    measureError'   <- (#peek QuantizeInfo, measure_error) ptr
    signature'      <- (#peek QuantizeInfo, signature) ptr
    ditherMethod'   <- (#peek QuantizeInfo, dither_method) ptr
    return QuantizeInfo { numberOfColors = numberOfColors'
                        , treeDepth      = treeDepth'
                        , shouldDither   = shouldDither'
                        , colorspace     = colorspace'
                        , measureError   = measureError'
                        , signature      = signature'
                        , ditherMethod   = ditherMethod'
                        }
  poke = error "not yet implemented"

