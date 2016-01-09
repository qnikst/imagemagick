{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.ImageMagick.MagickCore.Types.FFI.Types
    where

import           Data.Int
import           Data.Word
#include <magick/MagickCore.h>

type MagickRealType   = #type MagickRealType
type MagickStatusType = #type MagickStatusType
type MagickOffsetType = #type MagickOffsetType
type MagickSizeType   = #type MagickSizeType
type SignedQuantum    = #type SignedQuantum
type QuantumAny       = #type QuantumAny
type Quantum          = #type Quantum
type IndexPacket      = #type IndexPacket

magickEpsilon :: forall a. Fractional a => a
magickEpsilon   = 1e-10 -- #const MagickEpsilon
maxColormapSize :: forall a. Num a => a
maxColormapSize = #const MaxColormapSize
maxMap :: forall a. Num a => a
maxMap          = #const MaxMap
quantumFormat :: forall a. Num a => a
quantumFormat   = #const QuantumFormat
quantumRange :: forall a. Num a => a
quantumRange    = #const QuantumRange
