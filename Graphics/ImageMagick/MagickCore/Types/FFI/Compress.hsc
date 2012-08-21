{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.Types.FFI.Compress
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype CompressionType = CompressionType { unCompressionType :: CInt }
          deriving (Eq, Show)

#{enum CompressionType, CompressionType
  , undefinedCompression = UndefinedCompression
  , noCompression = NoCompression
  , bzipCompression = BZipCompression
  , dxt1Compression = DXT1Compression
  , dxt3Compression = DXT3Compression
  , dxt5Compression = DXT5Compression
  , axCompression = FaxCompression
  , group4Compression = Group4Compression
  , jpegCompression = JPEGCompression
  , jpeg2000Compression = JPEG2000Compression      /* ISO/IEC std 15444-1 */
  , losslessJPEGCompression = LosslessJPEGCompression
  , lzwCompression = LZWCompression
  , rleCompression = RLECompression
  , zipCompression = ZipCompression
  , zipsCompression = ZipSCompression
  , pizCompression = PizCompression
  , pxr24Compression = Pxr24Compression
  , b44Compression = B44Compression
  , b44aCompression = B44ACompression
  , lzmaCompression = LZMACompression            /* Lempel-Ziv-Markov chain algorithm */
  , jbig1Compression = JBIG1Compression           /* ISO/IEC std 11544 / ITU-T rec T.82 */
  , jbig2Compression = JBIG2Compression            /* ISO/IEC std 14492 / ITU-T rec T.88 */
}
