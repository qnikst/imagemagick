{-# LANGUAGE PackageImports, FlexibleContexts #-}
module Graphics.ImageMagick.MagickWand.MagickWand
  ( withMagickWandGenesis
  , magickWand
  , cloneMagickWand
  , magickIterate
  , resizeImage  -- TODO move to image
  , readImage 
  , writeImages
  , getImageHeight -- TODO move to image
  , quantumRange
  , MagickRealType
  -- move to another file
  ) where

import Prelude hiding (FilePath)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Graphics.ImageMagick.MagickWand.FFI.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.MagickWand  as F
import qualified Graphics.ImageMagick.MagickWand.FFI.WandImage   as F
import Filesystem.Path.CurrentOS
import Data.ByteString
import Foreign
import Foreign.C 

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
  where destroy x = F.destroyMagickWand x >> return ()

magickIterate :: (MonadResource m) => Ptr MagickWand -> (Ptr MagickWand -> m ()) -> m ()
magickIterate w f = liftIO (F.magickResetIterator w) >> go -- TODO: use fix
  where 
    go = do
      i <- liftIO $ F.magickNextImage w
      unless (i==mTrue) $ f w >> go


cloneMagickWand :: (MonadResource m) => Ptr MagickWand -> m (ReleaseKey, Ptr MagickWand)
cloneMagickWand w = allocate (F.cloneMagickWand w) destroy
  where destroy x = F.destroyMagickWand x >> return ()

resizeImage :: (MonadResource m) => Ptr MagickWand -> CInt -> CInt -> FilterTypes -> CDouble -> m Bool
resizeImage pw w h f s = liftIO $ F.magickResizeImage pw w h f s >>= return . (==mTrue)

readImage :: (MonadResource m) => Ptr MagickWand -> FilePath -> m Bool
readImage w fn = liftIO $ do
      x <- useAsCString (encode fn) (F.magickReadImage w)
      return (x==mTrue)

writeImages :: (MonadResource m) => Ptr MagickWand -> FilePath -> Bool -> m Bool
writeImages w fn b = liftIO $ do
  x <- useAsCString (encode fn) (\f -> F.magickWriteImages w f b')
  return (x==mTrue)
  where b' = if b then mTrue else mFalse

getImageHeight :: (MonadResource m) => Ptr MagickWand -> m Int                    -- TODO move to another file
getImageHeight w = liftIO $ fmap fromIntegral (F.magickGetImageHeight w)
