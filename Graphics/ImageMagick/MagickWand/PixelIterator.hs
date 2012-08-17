module Graphics.ImageMagick.MagickWand.PixelIterator
  ( pixelIterator
  , pixelRegionIterator
  , pixelGetNextIteratorRow
  , pixelSyncIterator
  , pixelResetIterator
  , pixelIterateList
  , pixelGetMagickColor     -- TODO move to another file
  , pixelSetMagickColor     -- TODO move to another file
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Vector.Storable                              (Vector)
import qualified Data.Vector.Storable                              as V
import           Foreign                                           hiding (void)
import           Foreign.C.Types                                   (CSize)
import           System.IO.Unsafe                                  (unsafeInterleaveIO)

import qualified Graphics.ImageMagick.MagickWand.FFI.PixelIterator as F
import qualified Graphics.ImageMagick.MagickWand.FFI.PixelWand     as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils


pixelIterator :: (MonadResource m) => Ptr MagickWand -> m (ReleaseKey, PPixelIterator)
pixelIterator w = allocate (F.newPixelIterator w) destroy
  where destroy = void . F.destroyPixelIterator

pixelRegionIterator :: (MonadResource m)
  => Ptr MagickWand -> Int -> Int -> Int -> Int -> m (ReleaseKey, PPixelIterator)
pixelRegionIterator w x y width height =  allocate (F.newPixelRegionIterator w x' y' width' height') destroy
  where destroy = void . F.destroyPixelIterator
        x' = fromIntegral x
        y' = fromIntegral y
        width' = fromIntegral width
        height' = fromIntegral height

pixelGetNextIteratorRow :: (MonadResource m) => PPixelIterator  -> m (Maybe (Vector PPixelWand))
pixelGetNextIteratorRow p = do
    x <- allocate (createPixelWandVector (F.pixelGetNextIteratorRow p)) (const $ return ())
    case x of
      (_, Just v) -> return (Just v)
      (_, Nothing) -> return Nothing

pixelGetMagickColor :: (MonadIO m) => PPixelWand -> m PPixelPacket
pixelGetMagickColor w = liftIO $ do
          c <- mallocForeignPtr
          withForeignPtr c (F.pixelGetMagickColor w)
          return c

pixelSetMagickColor :: (MonadResource m) => PPixelWand -> PPixelPacket -> m ()
pixelSetMagickColor w c = liftIO $ withForeignPtr c (F.pixelSetMagickColor w)

pixelSyncIterator :: (MonadResource m) => PPixelIterator -> m ()
pixelSyncIterator p =  withException_ p $ F.pixelSyncIterator p


pixelResetIterator :: (MonadResource m) => PPixelIterator -> m ()
pixelResetIterator = liftIO . F.pixelResetIterator


pixelIterateList :: (MonadResource m) => PPixelIterator -> m [Vector PPixelWand]
pixelIterateList it = pixelResetIterator it >> liftIO go
  where
    go :: IO [Vector PPixelWand]
    go = unsafeInterleaveIO $ do
          mv <- createPixelWandVector (F.pixelGetNextIteratorRow it)
          case mv of
            Just v -> go >>= return . (:) v
            Nothing -> return []


createPixelWandVector :: (Ptr CSize -> IO (Ptr PPixelWand)) -> IO (Maybe (Vector (PPixelWand)))
createPixelWandVector f = alloca $ \x -> do
          ptr <- f x
          if ptr == nullPtr
              then return Nothing
              else do
                  n   <- fmap fromIntegral (peek x)
                  fmap (Just . (\p -> V.unsafeFromForeignPtr0 p n)) (newForeignPtr_ ptr)

