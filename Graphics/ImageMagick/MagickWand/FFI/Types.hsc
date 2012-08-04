{-# LANGUAGE CPP, ForeignFunctionInterface, NoMonomorphismRestriction, RankNTypes #-}

module Graphics.ImageMagick.MagickWand.FFI.Types
  where

#include <wand/MagickWand.h>

import Foreign
import Foreign.C.Types

data PixelIterator
data MagickWand
data PixelWand
data DrawingWand
data Image

type MagickRealType = #type MagickRealType
type MagickStatusType = #type MagickStatusType
type MagickOffsetType = #type MagickOffsetType
type MagickSizeType   = #type MagickSizeType
type SignedQuantum    = #type SignedQuantum
type QuantumAny       = #type QuantumAny
type Quantum          = #type  Quantum

magickEpsilon :: forall a. Fractional a => a
magickEpsilon   = 1e-10 -- #const MagickEpsilon
magickHuge :: forall a. Num a => a
magickHuge      = #const MagickHuge
maxColormapSize :: forall a. Num a => a
maxColormapSize = #const MaxColormapSize
maxMap :: forall a. Num a => a
maxMap          = #const MaxMap
quantumFormat :: forall a. Num a => a
quantumFormat   = #const QuantumFormat
quantumRange :: forall a. Num a => a
quantumRange    = #const QuantumRange


newtype ChannelType = ChannelType { unChannelType :: CInt }
  deriving (Eq, Show)

#{enum ChannelType, ChannelType
 , undefinedCHannel =  UndefinedChannel
 , redChannel =  RedChannel
 , grayChannel = GrayChannel
 , cyanChannel = CyanChannel
 , greenChannel = GreenChannel
 , magentaChannel = MagentaChannel
 , blueChannel = BlueChannel
 , yellowChannel = YellowChannel
 , alphaChannel = AlphaChannel
 , opacityChannel = OpacityChannel
 , matteChannel = MatteChannel 
 , blackChannel = BlackChannel
 , indexChannel = IndexChannel
 , compositeChannels = CompositeChannels
 , allChannels = AllChannels
 , trueAlphaChannel = TrueAlphaChannel
 , rgbChannels = RGBChannels
 , grayChannels = GrayChannels
 , syncChannels = SyncChannels
 , defaultChannels = DefaultChannels
}

newtype AlphaChannelType = AlphaChannelType { unAlphaChannelType :: CInt }
  deriving (Eq, Show)

#{enum AlphaChannelType, AlphaChannelType
  , undefinedAlphaChannel = UndefinedAlphaChannel
  , activateAlphaChannel = ActivateAlphaChannel
  , backgroundAlphaChannel = BackgroundAlphaChannel
  , copyAlphaChannel = CopyAlphaChannel
  , deactivateAlphaChannel = DeactivateAlphaChannel
  , extractAlphaChannel = ExtractAlphaChannel
  , opaqueAlphaChannel = OpaqueAlphaChannel
  , resetAlphaChannel = ResetAlphaChannel  /* deprecated */
  , setAlphaChannel = SetAlphaChannel
  , shapeAlphaChannel = ShapeAlphaChannel
  , transparentAlphaChannel = TransparentAlphaChannel
  , lattenAlphaChannel = FlattenAlphaChannel
  , removeAlphaChannel = RemoveAlphaChannel
}

newtype FilterTypes = FilterTypes { unPCREOption :: CInt }
    deriving (Eq,Show)

#{enum FilterTypes, FilterTypes
  , undefinedFilter = UndefinedFilter
  , pointFilter     = PointFilter
  , boxFilter       = BoxFilter
  , triangleFilter  = TriangleFilter
  , hermiteFilter   = HermiteFilter
  , hanningFilter   = HanningFilter
  , hammingFilter   = HammingFilter
  , blackmanFilter  = BlackmanFilter
  , gaussianFilter  = GaussianFilter
  , qaudraticFilter = QuadraticFilter
  , cubicFilter     = CubicFilter
  , catromFilter    = CatromFilter
  , mirchellFilter  = MitchellFilter
  , jincFilter      = JincFilter
  , sinkFilter      = SincFilter
  , sinkFastFilter  = SincFastFilter
  , kaiserFilter    = KaiserFilter
  , welshFilter     = WelshFilter
  , parzenFilter    = ParzenFilter
  , bohmanFilter    = BohmanFilter
  , bartlettFilter  = BartlettFilter
  , lagrangeFilter  = LagrangeFilter
  , lanczosFilter   = LanczosFilter
  , lanczosSharpFilter = LanczosSharpFilter
  , lanczos2Filter  = Lanczos2Filter
  , lanczos2SharpFilter = Lanczos2SharpFilter
  , robidouxFilter  = RobidouxFilter
}

newtype MagickBooleanType = MagickBooleanType { unMagickBooleanType :: CInt}
  deriving (Eq, Show)

#{enum MagickBooleanType, MagickBooleanType 
  , mFalse = MagickFalse
  , mTrue  = MagickTrue
}

newtype ClassType = ClassType { unClassType :: CInt}
  deriving (Eq, Show)

#{enum ClassType, ClassType
  , undefinedClass = UndefinedClass
  , directClass    = DirectClass
  , pseudoClass    = PseudoClass
}

data MagickPixelPacket 

instance Storable MagickPixelPacket where
  sizeOf = const #size MagickPixelPacket
  alignment _ = 1

getPixelRed   = #peek MagickPixelPacket, red
getPixelGreen = #peek MagickPixelPacket, green
getPixelBlue  = #peek MagickPixelPacket, blue
getPixelIndex = #peek MagickPixelPacket, index
setPixelRed   = #poke MagickPixelPacket, red
setPixelGreen = #poke MagickPixelPacket, green
setPixelBlue  = #poke MagickPixelPacket, blue
setPixelIndex = #poke MagickPixelPacket, index

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
