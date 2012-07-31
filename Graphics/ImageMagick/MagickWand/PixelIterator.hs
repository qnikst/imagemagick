{-# LANGUAGE PackageImports #-}
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

import Foreign
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Graphics.ImageMagick.MagickWand.FFI.Types
import Graphics.ImageMagick.MagickWand.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.PixelIterator as F
import qualified Graphics.ImageMagick.MagickWand.FFI.PixelWand     as F


pixelIterator :: (MonadResource m) => Ptr MagickWand -> m (ReleaseKey, PPixelIterator) 
pixelIterator w = allocate (F.newPixelIterator w) destroy
  where destroy x = F.destroyPixelIterator x >> return ()


pixelGetNextIteratorRow :: (MonadResource m) => PPixelIterator  -> m (ReleaseKey, Vector PPixelWand)
pixelGetNextIteratorRow p = allocate create (const $ return (){-V.mapM_ F.destroyPixelWand-})
  where create = alloca $ \x -> do
                  ptr <- F.pixelGetNextIteratorRow p x
                  n   <- fmap fromIntegral (peek x)
                  fmap (flip V.unsafeFromForeignPtr0 n) (newForeignPtr_ ptr)

pixelGetMagickColor :: (MonadIO m) => PPixelWand -> m (PPixelPacket)
pixelGetMagickColor w = liftIO $ do 
          c <- mallocForeignPtr
          withForeignPtr c (F.pixelGetMagickColor w)
          return c

pixelSetMagickColor :: (MonadIO m) => PPixelWand -> PPixelPacket -> m ()
pixelSetMagickColor w c = liftIO $ withForeignPtr c (F.pixelSetMagickColor w)

pixelSyncIterator :: (MonadIO m) => PPixelIterator -> m (Bool)
pixelSyncIterator p =  liftIO (F.pixelSyncIterator p) >>= \x -> return  (x==mTrue)
