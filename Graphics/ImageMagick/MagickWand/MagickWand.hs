{-# LANGUAGE FlexibleContexts #-}
module Graphics.ImageMagick.MagickWand.MagickWand
  ( withMagickWandGenesis
  , localGenesis
  , magickWand
  , wandResource
  , cloneMagickWand
  , magickIterate
  , readImage
  , setSize
  , setImageArtifact
  , deleteImageArtifact
  , setIteratorIndex
  , resetIterator
  , setOption
  -- TODO: move somewhere
  , lanczosFilter
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString
import           Data.Text                                          (Text)
import           Data.Text.Encoding                                 (encodeUtf8)
import           Filesystem.Path.CurrentOS
import           Foreign                                            hiding (void)
import qualified Graphics.ImageMagick.MagickWand.FFI.MagickWand     as F
import           Graphics.ImageMagick.MagickWand.FFI.Types
import qualified Graphics.ImageMagick.MagickWand.FFI.WandProperties as F
import           Graphics.ImageMagick.MagickWand.Types
import           Graphics.ImageMagick.MagickWand.Utils
import           Prelude                                            hiding (FilePath)

-- | Create magic wand environment and closes it at the
-- end of the work, should wrap all MagickWand functions
-- withMagickWandGenesis :: IO a -> IO a
-- withMagickWandGenesis :: (MonadCatchIO m, MonadBaseControl IO m, MonadCatchIO (ResourceT IO)) => (ResourceT m c) -> m c
withMagickWandGenesis f = bracket start finish (\_ -> runResourceT f)
  where
    start = liftIO F.magickWandGenesis
    finish = liftIO . const F.magickWandTerminus

-- | Open a nested block inside genesis (for tracking nested resources)
localGenesis f = runResourceT f

magickWand :: (MonadResource m) => m (ReleaseKey, Ptr MagickWand)
magickWand = allocate F.newMagickWand destroy
  where destroy = void . F.destroyMagickWand

magickIterate :: (MonadResource m) => Ptr MagickWand -> (Ptr MagickWand -> m ()) -> m ()
magickIterate w f = liftIO (F.magickResetIterator w) >> go -- TODO: use fix
  where
    go = do
      i <- liftIO $ F.magickNextImage w
      unless (i==mTrue) $ f w >> go


wandResource :: (MonadResource m) => (IO (Ptr MagickWand)) -> m (ReleaseKey, Ptr MagickWand)
wandResource f = allocate f destroy
  where destroy = void . F.destroyMagickWand


cloneMagickWand :: (MonadResource m) => Ptr MagickWand -> m (ReleaseKey, Ptr MagickWand)
cloneMagickWand w = wandResource (F.cloneMagickWand w)


readImage :: (MonadResource m) => Ptr MagickWand -> FilePath -> m ()
readImage w fn = withException_ w $ useAsCString (encode fn) (F.magickReadImage w)


setSize :: (MonadResource m) => Ptr MagickWand -> Int -> Int -> m ()
setSize w cols rows = withException_ w $ F.magickSetSize w (fromIntegral cols) (fromIntegral rows)


-- | MagickSetImageArtifact() associates a artifact with an image.
-- The format of the MagickSetImageArtifact method is:
setImageArtifact :: (MonadResource m) => PMagickWand -> ByteString -> ByteString -> m () -- TODO use normal types
setImageArtifact w a v = withException_ w $ useAsCString a 
                                          $ \a' -> useAsCString v 
                                          $ F.magickSetImageArtifact w a'


-- | MagickDeleteImageArtifact() deletes a wand artifact.
deleteImageArtifact :: (MonadResource m) => PMagickWand -> ByteString -> m ()
deleteImageArtifact w a = withException_ w $ useAsCString a
                                           $ F.magickDeleteImageArtifact w


-- | Sets the iterator to the given position in the image list specified
-- with the index parameter. A zero index will set the first image as
-- current, and so on. Negative indexes can be used to specify an image
-- relative to the end of the images in the wand, with -1 being the last
-- image in the wand.
setIteratorIndex :: (MonadResource m) => Ptr MagickWand -> Int -> m ()
setIteratorIndex w i = withException_ w $ F.magickSetIteratorIndex w (fromIntegral i)


resetIterator :: (MonadResource m) => Ptr MagickWand -> m ()
resetIterator = liftIO . F.magickResetIterator


-- | Associates one or options with the wand (e.g. setOption wand "jpeg:perserve" "yes").
setOption :: (MonadResource m) => Ptr MagickWand -> Text -> Text -> m ()
setOption w key value =
  withException_ w $ useAsCString (encodeUtf8 key) $
  \cstr -> useAsCString (encodeUtf8 value) (F.magickSetOption w cstr)
