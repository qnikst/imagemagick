{-# LINE 1 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface, NoMonomorphismRestriction #-}
{-# LINE 2 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}

module Graphics.ImageMagick.MagickWand.FFI.Types
  where


{-# LINE 7 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}

import Foreign
import Foreign.C.Types
import Data.Word
import Data.Int

data PixelIterator
data MagickWand
data PixelWand
data Image

type MagickRealType = Double
{-# LINE 19 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
type MagickStatusType = Word32
{-# LINE 20 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
type MagickOffsetType = Int64
{-# LINE 21 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
type MagickSizeType   = Word64
{-# LINE 22 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
type SignedQuantum    = Int64
{-# LINE 23 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
type QuantumAny       = Word64
{-# LINE 24 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
type Quantum          = Word16
{-# LINE 25 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}

magickEpsilon   = 1e-10 -- #const MagickEpsilon
magickHuge      = 1000000000000
{-# LINE 28 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
maxColormapSize = 65536
{-# LINE 29 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
maxMap          = 65535
{-# LINE 30 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
quantumFormat   = 4204633
{-# LINE 31 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
quantumRange    = 65535
{-# LINE 32 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}


newtype ChannelType = ChannelType { unChannelType :: CInt }
  deriving (Eq, Show)

undefinedCHannel  :: ChannelType
undefinedCHannel  = ChannelType 0
redChannel  :: ChannelType
redChannel  = ChannelType 1
grayChannel  :: ChannelType
grayChannel  = ChannelType 1
cyanChannel  :: ChannelType
cyanChannel  = ChannelType 1
greenChannel  :: ChannelType
greenChannel  = ChannelType 2
magentaChannel  :: ChannelType
magentaChannel  = ChannelType 2
blueChannel  :: ChannelType
blueChannel  = ChannelType 4
yellowChannel  :: ChannelType
yellowChannel  = ChannelType 4
alphaChannel  :: ChannelType
alphaChannel  = ChannelType 8
opacityChannel  :: ChannelType
opacityChannel  = ChannelType 8
matteChannel  :: ChannelType
matteChannel  = ChannelType 8
blackChannel  :: ChannelType
blackChannel  = ChannelType 32
indexChannel  :: ChannelType
indexChannel  = ChannelType 32
compositeChannels  :: ChannelType
compositeChannels  = ChannelType 47
allChannels  :: ChannelType
allChannels  = ChannelType 134217727
trueAlphaChannel  :: ChannelType
trueAlphaChannel  = ChannelType 64
rgbChannels  :: ChannelType
rgbChannels  = ChannelType 128
grayChannels  :: ChannelType
grayChannels  = ChannelType 128
syncChannels  :: ChannelType
syncChannels  = ChannelType 256
defaultChannels  :: ChannelType
defaultChannels  = ChannelType 134217719

{-# LINE 59 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}

newtype FilterTypes = FilterTypes { unPCREOption :: CInt }
    deriving (Eq,Show)

undefinedFilter  :: FilterTypes
undefinedFilter  = FilterTypes 0
pointFilter      :: FilterTypes
pointFilter      = FilterTypes 1
boxFilter        :: FilterTypes
boxFilter        = FilterTypes 2
triangleFilter   :: FilterTypes
triangleFilter   = FilterTypes 3
hermiteFilter    :: FilterTypes
hermiteFilter    = FilterTypes 4
hanningFilter    :: FilterTypes
hanningFilter    = FilterTypes 5
hammingFilter    :: FilterTypes
hammingFilter    = FilterTypes 6
blackmanFilter   :: FilterTypes
blackmanFilter   = FilterTypes 7
gaussianFilter   :: FilterTypes
gaussianFilter   = FilterTypes 8
qaudraticFilter  :: FilterTypes
qaudraticFilter  = FilterTypes 9
cubicFilter      :: FilterTypes
cubicFilter      = FilterTypes 10
catromFilter     :: FilterTypes
catromFilter     = FilterTypes 11
mirchellFilter   :: FilterTypes
mirchellFilter   = FilterTypes 12
jincFilter       :: FilterTypes
jincFilter       = FilterTypes 13
sinkFilter       :: FilterTypes
sinkFilter       = FilterTypes 14
sinkFastFilter   :: FilterTypes
sinkFastFilter   = FilterTypes 15
kaiserFilter     :: FilterTypes
kaiserFilter     = FilterTypes 16
welshFilter      :: FilterTypes
welshFilter      = FilterTypes 17
parzenFilter     :: FilterTypes
parzenFilter     = FilterTypes 18
bohmanFilter     :: FilterTypes
bohmanFilter     = FilterTypes 19
bartlettFilter   :: FilterTypes
bartlettFilter   = FilterTypes 20
lagrangeFilter   :: FilterTypes
lagrangeFilter   = FilterTypes 21
lanczosFilter    :: FilterTypes
lanczosFilter    = FilterTypes 22
lanczosSharpFilter  :: FilterTypes
lanczosSharpFilter  = FilterTypes 23
lanczos2Filter   :: FilterTypes
lanczos2Filter   = FilterTypes 24
lanczos2SharpFilter  :: FilterTypes
lanczos2SharpFilter  = FilterTypes 25
robidouxFilter   :: FilterTypes
robidouxFilter   = FilterTypes 26

{-# LINE 92 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}

newtype MagickBooleanType = MagickBooleanType { unMagickBooleanType :: CInt}
  deriving (Eq, Show)

mFalse  :: MagickBooleanType
mFalse  = MagickBooleanType 0
mTrue   :: MagickBooleanType
mTrue   = MagickBooleanType 1

{-# LINE 100 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}

newtype ClassType = ClassType { unClassType :: CInt}
  deriving (Eq, Show)

undefinedClass  :: ClassType
undefinedClass  = ClassType 0
directClass     :: ClassType
directClass     = ClassType 1
pseudoClass     :: ClassType
pseudoClass     = ClassType 2

{-# LINE 109 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}

data MagickPixelPacket 

instance Storable MagickPixelPacket where
  sizeOf = const (72)
{-# LINE 114 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
  alignment _ = 1

getPixelRed   = (\hsc_ptr -> peekByteOff hsc_ptr 32)
{-# LINE 117 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
getPixelGreen = (\hsc_ptr -> peekByteOff hsc_ptr 40)
{-# LINE 118 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
getPixelBlue  = (\hsc_ptr -> peekByteOff hsc_ptr 48)
{-# LINE 119 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
getPixelIndex = (\hsc_ptr -> peekByteOff hsc_ptr 64)
{-# LINE 120 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
setPixelRed   = (\hsc_ptr -> pokeByteOff hsc_ptr 32)
{-# LINE 121 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
setPixelGreen = (\hsc_ptr -> pokeByteOff hsc_ptr 40)
{-# LINE 122 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
setPixelBlue  = (\hsc_ptr -> pokeByteOff hsc_ptr 48)
{-# LINE 123 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}
setPixelIndex = (\hsc_ptr -> pokeByteOff hsc_ptr 64)
{-# LINE 124 "Graphics/ImageMagick/MagickWand/FFI/Types.hsc" #-}

{-
data MagicPixelPacket = MagickPixelPacket 
  { storageClass :: ClassType
  , colorspace   :: ColorspaceType
  , matte        :: MagickBooleanType
  , fuzz         :: Double
  , depth        :: CSize
  , red          :: MagickRealType
  , green        :: MagickRealType
  , blue         :: MagickRealType
  , opacity      :: MagickRealType
  , index        :: MagickRealType
  }
  -}
