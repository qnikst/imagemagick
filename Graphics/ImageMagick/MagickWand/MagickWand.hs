{-# LANGUAGE FlexibleContexts #-}
module Graphics.ImageMagick.MagickWand.MagickWand
  ( withMagickWandGenesis
  , localGenesis
  -- * Creation
  , magickWand
  , wandResource
--  , magickWandFromImage
--  , isMagickWand 
  , cloneMagickWand
--  , clearMagickWand 
  -- * Iratation 
  , magickIterate
  , magickIterateReverse
--  , getIteratorIndex 
  , readImage
  , getSize
  , setSize
  , setImageArtifact
  , deleteImageArtifact
  , getIteratorIndex
  , setIteratorIndex
--  , setFirstIterator 
--  , setLastIterator 
  , resetIterator
  , magickIterate
  , setOption
--  , queryConfigureOption 
--  , queryConfigureOptions 
--  , queryFontMetrics 
--  , queryMultilineFontMetrics 
--  , queryFonts 
--  , relinquishMemory 
  , setImageArtifact
--  , deleteImageArtifact
--    , deleteImageProperty 
--    , deleteOption 
--    , getAntialias 
--    , getBackgroundColor 
--    , getColorspace 
--    , getCompression 
--    , getCompressionQuality 
--    , getCopyright 
--    , getFilename 
--    , getFont 
--    , getFormat 
--    , getGravity 
--    , getHomeURL 
--    , getImageArtifact 
--    , getImageArtifacts 
--    , getImageProfile 
--   , getImageProfiles 
--   , getImageProperty 
--    , getImageProperties 
--    , getInterlaceScheme 
--    , getInterpolateMethod 
--    , getOption 
--    , getOptions 
--    , getOrientation 
--    , getPackageName 
--    , getPage 
--    , getPointsize 
--    , getQuantumDepth 
--    , getQuantumRange 
--    , getReleaseDate 
--    , getResolution 
--    , getResource 
--    , getResourceLimit 
--    , getSamplingFactors 
--    , getSize 
--    , getSizeOffset 
--    , getType 
--    , getVersion 
--    , profileImage 
--    , removeImageProfile 
--    , setAntialias 
--    , setBackgroundColor 
--    , setColorspace 
--    , setCompression 
--    , setCompressionQuality 
--    , setDepth 
--    , setExtract 
--    , setFilename 
--    , setFont 
--    , setFormat 
--    , setGravity 
--    , setImageArtifact 
--    , setImageProfile 
--    , setImageProperty 
--    , setInterlaceScheme 
--    , setInterpolateMethod 
--    , setOption 
--    , setOrientation 
--    , setPage 
--    , setPassphrase 
--    , setPointsize 
--    , setProgressMonitor 
--    , setResourceLimit 
--    , setResolution 
--    , setSamplingFactors 
--    , setSizeOffset 
--    , setType
  ) where

import           Control.Applicative                                ((<$>))
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
magickWand = wandResource F.newMagickWand

magickIterateF :: (MonadResource m) =>
                 (PMagickWand -> IO MagickBooleanType) -> PMagickWand -> (PMagickWand -> m ()) -> m ()
magickIterateF nf w f = liftIO (F.magickResetIterator w) >> go -- TODO: use fix
  where
    go = do
      i <- liftIO $ nf w
      unless (i==mTrue) $ f w >> go

magickIterate :: (MonadResource m) => Ptr MagickWand -> (Ptr MagickWand -> m ()) -> m ()
magickIterate = magickIterateF F.magickNextImage

magickIterateReverse :: (MonadResource m) => Ptr MagickWand -> (Ptr MagickWand -> m ()) -> m ()
magickIterateReverse = magickIterateF F.magickPreviousImage

wandResource :: (MonadResource m) => (IO (Ptr MagickWand)) -> m (ReleaseKey, Ptr MagickWand)
wandResource f = allocate f destroy
  where destroy = void . F.destroyMagickWand

cloneMagickWand :: (MonadResource m) => Ptr MagickWand -> m (ReleaseKey, Ptr MagickWand)
cloneMagickWand = wandResource . F.cloneMagickWand

readImage :: (MonadResource m) => Ptr MagickWand -> FilePath -> m ()
readImage w fn = withException_ w $ useAsCString (encode fn) (F.magickReadImage w)

setSize :: (MonadResource m) => Ptr MagickWand -> Int -> Int -> m ()
setSize w cols rows = withException_ w $ F.magickSetSize w (fromIntegral cols) (fromIntegral rows)

-- | Returns the size associated with the magick wand.
getSize :: (MonadResource m) => Ptr MagickWand -> m (Int, Int)
getSize w = liftIO $ alloca $ \pw -> do
  height <- alloca $ \ph -> F.magickGetSize w pw ph >> peek ph >>= return
  width <- peek pw
  return (fromIntegral width, fromIntegral height)


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

-- | Returns the position of the iterator in the image list.
getIteratorIndex :: (MonadResource m) => Ptr MagickWand -> m Int
getIteratorIndex w = liftIO $ fromIntegral <$> F.magickGetIteratorIndex w

resetIterator :: (MonadResource m) => Ptr MagickWand -> m ()
resetIterator = liftIO . F.magickResetIterator

-- | Associates one or options with the wand (e.g. setOption wand "jpeg:perserve" "yes").
setOption :: (MonadResource m) => Ptr MagickWand -> Text -> Text -> m ()
setOption w key value =
  withException_ w $ useAsCString (encodeUtf8 key) $
  \cstr -> useAsCString (encodeUtf8 value) (F.magickSetOption w cstr)
