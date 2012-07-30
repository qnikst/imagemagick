{-# LANGUAGE PackageImports #-}
module Graphics.ImageMagick.MagickWand where

import Prelude hiding (FilePath)
import "mtl" Control.Monad.Reader                         -- TODO: remove
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Graphics.ImageMagick.MagickWand.Internal
import Filesystem.Path.CurrentOS
import Data.ByteString
import Foreign
import Foreign.C 

type PMagickWand = Ptr MagickWand

-- | Create magic wand environment and closes it at the
-- end of the work, should wrap all MagickWand functions
-- withMagickWandGenesis :: IO a -> IO a
withMagickWandGenesis f = bracket start finish (const $ runResourceT f)
  where
    start = magickWandGenesis
    finish = const magickWandTerminus

magickWand :: (MonadResource m) => m (ReleaseKey, Ptr MagickWand)
magickWand = allocate newMagickWand destroy 
  where destroy x = destroyMagickWand x >> return ()

magickIterate :: (MonadResource m) => PMagickWand -> (Ptr MagickWand -> m ()) -> m ()
magickIterate w f = liftIO (magickResetIterator w) >> go w f -- TODO: use fix
  where 
    go w f = do
      i <- liftIO $ magickNextImage w
      unless (i==0) $ f w >> go w f

resizeImage :: (MonadResource m) => Ptr MagickWand -> CInt -> CInt -> FilterTypes -> CDouble -> m Bool
resizeImage pw w h f s = liftIO $  magickResizeImage pw w h f s >>= return . (/=0)

readImage :: (MonadResource m) => PMagickWand -> FilePath -> m Bool
readImage w fn = liftIO $ do
      x <- useAsCString (encode fn) (magickReadImage w)
      return (x/=0)

writeImages :: (MonadResource m) => PMagickWand -> FilePath -> Bool -> m Bool
writeImages w fn b = liftIO $ do
  x <- useAsCString (encode fn) (\f -> magickWriteImages w f b')
  return (x/=0)
  where b' = if b then 1 else 0

