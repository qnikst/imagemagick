{-# LANGUAGE FlexibleContexts #-}
module Graphics.ImageMagick.MagickWand.MagickWand
  ( withMagickWandGenesis
  , magickWand
  , cloneMagickWand
  , magickIterate
  , readImage
  , writeImages
  , quantumRange
  , setSize
  -- TODO: move somewhere
  , lanczosFilter
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString
import           Filesystem.Path.CurrentOS
import           Foreign                                        hiding (void)
import qualified Graphics.ImageMagick.MagickWand.FFI.MagickWand as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils
import           Prelude                                        hiding (FilePath)

-- | Create magic wand environment and closes it at the
-- end of the work, should wrap all MagickWand functions
-- withMagickWandGenesis :: IO a -> IO a
-- withMagickWandGenesis :: (MonadCatchIO m, MonadBaseControl IO m, MonadCatchIO (ResourceT IO)) => (ResourceT m c) -> m c
withMagickWandGenesis f = bracket start finish (\_ -> runResourceT f)
  where
    start = liftIO F.magickWandGenesis
    finish = liftIO . const F.magickWandTerminus

magickWand :: (MonadResource m) => m (ReleaseKey, Ptr MagickWand)
magickWand = allocate F.newMagickWand destroy
  where destroy = void . F.destroyMagickWand

magickIterate :: (MonadResource m) => Ptr MagickWand -> (Ptr MagickWand -> m ()) -> m ()
magickIterate w f = liftIO (F.magickResetIterator w) >> go -- TODO: use fix
  where
    go = do
      i <- liftIO $ F.magickNextImage w
      unless (i==mTrue) $ f w >> go


cloneMagickWand :: (MonadResource m) => Ptr MagickWand -> m (ReleaseKey, Ptr MagickWand)
cloneMagickWand w = allocate (F.cloneMagickWand w) destroy
  where destroy = void . F.destroyMagickWand


readImage :: (MonadResource m) => Ptr MagickWand -> FilePath -> m Bool
readImage w fn = liftIO $ do
      x <- useAsCString (encode fn) (F.magickReadImage w)
      return (x==mTrue)

writeImages :: (MonadResource m) => Ptr MagickWand -> FilePath -> Bool -> m Bool
writeImages w fn b = liftIO $ do
  x <- useAsCString (encode fn) (\f -> F.magickWriteImages w f b')
  return (x==mTrue)
  where b' = if b then mTrue else mFalse

setSize :: (MonadResource m) => Ptr MagickWand -> Int -> Int -> m Bool
setSize w cols rows = fromMBool $ F.magickSetSize w (fromIntegral cols) (fromIntegral rows)


