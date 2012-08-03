module Graphics.ImageMagick.MagickWand.PixelIterator
  ( pixelIterator
  , pixelGetNextIteratorRow
  , pixelSyncIterator
  , PPixelIterator
  , PPixelWand
  , PPixelPacket
  , pixelGetMagickColor     -- TODO move to another file
  , pixelSetMagickColor     -- TODO move to another file
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Vector.Storable                              (Vector)
import qualified Data.Vector.Storable                              as V
import           Foreign                                           hiding (void)

import qualified Graphics.ImageMagick.MagickWand.FFI.PixelIterator as F
import qualified Graphics.ImageMagick.MagickWand.FFI.PixelWand     as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import           Graphics.ImageMagick.MagickWand.Types


pixelIterator :: (MonadResource m) => Ptr MagickWand -> m (ReleaseKey, PPixelIterator)
pixelIterator w = allocate (F.newPixelIterator w) destroy
  where destroy = void . F.destroyPixelIterator


pixelGetNextIteratorRow :: (MonadResource m) => PPixelIterator  -> m (ReleaseKey, Vector PPixelWand)
pixelGetNextIteratorRow p = allocate create (const $ return ())
  where create = alloca $ \x -> do
                  ptr <- F.pixelGetNextIteratorRow p x
                  n   <- fmap fromIntegral (peek x)
                  fmap (`V.unsafeFromForeignPtr0` n) (newForeignPtr_ ptr)

pixelGetMagickColor :: (MonadIO m) => PPixelWand -> m PPixelPacket
pixelGetMagickColor w = liftIO $ do
          c <- mallocForeignPtr
          withForeignPtr c (F.pixelGetMagickColor w)
          return c

pixelSetMagickColor :: (MonadIO m) => PPixelWand -> PPixelPacket -> m ()
pixelSetMagickColor w c = liftIO $ withForeignPtr c (F.pixelSetMagickColor w)

pixelSyncIterator :: (MonadIO m) => PPixelIterator -> m Bool
pixelSyncIterator p =  liftIO (F.pixelSyncIterator p) >>= \x -> return  (x==mTrue)
