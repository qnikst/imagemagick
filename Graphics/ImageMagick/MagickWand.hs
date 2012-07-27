{-# LANGUAGE PackageImports #-}
module Graphics.ImageMagick.MagickWand where

import Prelude hiding (FilePath)
import "mtl" Control.Monad.Reader                         -- TODO: remove
import Control.Exception
import Control.Monad.IO.Class
import Graphics.ImageMagick.MagickWand.Internal
import Filesystem.Path.CurrentOS
import Data.ByteString
import Foreign
import Foreign.C 

data Filter                                                 -- TODO: change name
type PMagickWand = Ptr MagickWand
type MagickR a = ReaderT PMagickWand IO a

-- | Create magic wand environment and closes it at the
-- end of the work, should wrap all MagickWand functions
withMagickWandGenesis :: IO a -> IO a
withMagickWandGenesis f = bracket start finish (const f)
  where
    start = magickWandGenesis
    finish = const magickWandTerminus

withMagickWand :: (MagickR a) -> IO a
withMagickWand f = bracket start close (runReaderT f)
  where
    start = newMagickWand
    close = destroyMagickWand


magickIterate :: (MagickR a) -> (MagickR ())
magickIterate f = ask >>= \w -> liftIO (magickResetIterator w) >> go w f -- TODO: use fix
  where 
    go w f = do
      i <- lift (magickNextImage w)
      unless (i==0) $ f >> go w f

resizeImage :: CInt -> CInt -> FilterTypes -> CDouble -> MagickR Bool
resizeImage w h f s = ask >>= \m -> fmap (/=0) $! lift (magickResizeImage m w h f s)

readImage :: FilePath -> MagickR Bool                       -- TODO: move to apropiate file
readImage fn = do 
  w <- ask
  r <- lift $ useAsCString (encode fn) (magickReadImage w)
  return (r/=0)

writeImages :: FilePath -> Bool -> MagickR Bool
writeImages fn b = do
  w <- ask 
  r <- lift $ useAsCString (encode fn) (\f -> magickWriteImages w f b')
  return (r/=0)
  where b' = if b then 1 else 0

