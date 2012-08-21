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
  , deleteOption
  , getOption
  , setOption
  , getOptions
  , deleteImageProperty
  , getImageProperty
  , setImageProperty
  , getImageProperties
  , setImageFormat
  , getImageProfile
  , removeImageProfile
  , setImageProfile
  , getImageProfiles
--  , queryConfigureOption 
--  , queryConfigureOptions 
--  , queryFontMetrics 
--  , queryMultilineFontMetrics 
--  , queryFonts 
--  , relinquishMemory 
--  , deleteImageArtifact
--    , deleteImageProperty 
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
--    , getInterlaceScheme 
--    , getInterpolateMethod 
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
--    , setInterlaceScheme 
--    , setInterpolateMethod 
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
import           Data.Text.Encoding                                 (decodeUtf8, encodeUtf8)
import           Data.Vector.Storable                               (Vector)
import qualified Data.Vector.Storable                               as V
import           Filesystem.Path.CurrentOS
import           Foreign                                            hiding (void)
import           Foreign.C.String
import           Foreign.C.Types
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
                 (PMagickWand -> IO ())
                 -> (PMagickWand -> IO MagickBooleanType)
                 -> PMagickWand -> (PMagickWand -> m ()) -> m ()
magickIterateF initF next w f = liftIO (initF w) >> go -- TODO: use fix
  where
    go = do
      i <- liftIO $ next w
      when (i==mTrue) $ f w >> go

magickIterate :: (MonadResource m) => Ptr MagickWand -> (Ptr MagickWand -> m ()) -> m ()
magickIterate = magickIterateF F.magickResetIterator F.magickNextImage

magickIterateReverse :: (MonadResource m) => Ptr MagickWand -> (Ptr MagickWand -> m ()) -> m ()
magickIterateReverse = magickIterateF F.magickSetLastIterator F.magickPreviousImage

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

getOption :: (MonadResource m) => Ptr MagickWand -> Text -> m Text
getOption w key = liftIO $ do
  cstr <- useAsCString (encodeUtf8 key) (F.magickGetOption w)
  value <- decodeUtf8 <$> packCString cstr
  F.magickRelinquishMemory (castPtr cstr)
  return value

-- | Associates one or options with the wand (e.g. setOption wand "jpeg:perserve" "yes").
deleteOption :: (MonadResource m) => Ptr MagickWand -> Text -> m ()
deleteOption w key =
  withException_ w $ useAsCString (encodeUtf8 key) (F.magickDeleteOption w)

-- | Associates one or options with the wand (e.g. setOption wand "jpeg:perserve" "yes").
setOption :: (MonadResource m) => Ptr MagickWand -> Text -> Text -> m ()
setOption w key value =
  withException_ w $ useAsCString (encodeUtf8 key) $
  \cstr -> useAsCString (encodeUtf8 value) (F.magickSetOption w cstr)

setImageFormat :: (MonadResource m) => Ptr MagickWand -> Text -> m ()
setImageFormat w format =
  withException_ w $ useAsCString (encodeUtf8 format) (F.magickSetImageFormat w)

getOptions :: (MonadResource m) => Ptr MagickWand -> Text -> m [Text]
getOptions w pattern = liftIO $ alloca $ \pn -> do
  poptionps <- useAsCString (encodeUtf8 pattern) (\cstr -> F.magickGetOptions w cstr pn)
  n <- fromIntegral <$> peek pn
  optionps <- peekArray n poptionps
  options <- forM optionps $ \optionp -> do
    option <- decodeUtf8 <$> packCString optionp
    F.magickRelinquishMemory (castPtr optionp)
    return option
  F.magickRelinquishMemory (castPtr poptionps)
  return options

-- | Deletes a wand property
deleteImageProperty :: (MonadResource m) => Ptr MagickWand -> Text -> m ()
deleteImageProperty w prop =
  withException_ w $ useAsCString (encodeUtf8 prop) (F.magickDeleteImageProperty w)

-- | Returns a value associated with the specified property
getImageProperty :: (MonadResource m) => Ptr MagickWand -> Text -> m Text
getImageProperty w prop = liftIO $ do
  cstr <- useAsCString (encodeUtf8 prop) (F.magickGetImageProperty w)
  value <- decodeUtf8 <$> packCString cstr
  F.magickRelinquishMemory (castPtr cstr)
  return value

-- | Associates a property with an image.
setImageProperty :: (MonadResource m) => Ptr MagickWand -> Text -> Text -> m ()
setImageProperty w prop value =
  withException_ w $ useAsCString (encodeUtf8 prop) $
  \cstr -> useAsCString (encodeUtf8 value) (F.magickSetImageProperty w cstr)

-- | Returns all the property names that match the specified pattern associated
-- with a wand
getImageProperties :: (MonadResource m) => Ptr MagickWand -> Text -> m [Text]
getImageProperties w pattern = liftIO $ alloca $ \pn -> do
  ppropps <- useAsCString (encodeUtf8 pattern) (\cstr -> F.magickGetImageProperties w cstr pn)
  n <- fromIntegral <$> peek pn
  propps <- peekArray n ppropps
  props <- forM propps $ \propp -> do
    prop <- decodeUtf8 <$> packCString propp
    F.magickRelinquishMemory (castPtr propp)
    return prop
  F.magickRelinquishMemory (castPtr ppropps)
  return props

getProfile :: (MonadResource m) =>
              (PMagickWand -> CString ->  Ptr CSize -> IO (Ptr Word8)) ->
              PMagickWand -> Text -> m (Vector Word8)
getProfile f w name = liftIO $ do
  (pprofile, len) <- alloca $ \pn -> useAsCString (encodeUtf8 name) $ \cstr -> do
    p <- f w cstr pn
    n <- fromIntegral <$> peek pn
    return (p,n)
  -- TODO: maybe we should use copyBytes instead?
  profile <- V.generateM len (peekElemOff pprofile)
  F.magickRelinquishMemory (castPtr pprofile)
  return profile

-- | Returns the named image profile.
getImageProfile :: (MonadResource m) => Ptr MagickWand -> Text -> m (Vector Word8)
getImageProfile = getProfile F.magickGetImageProfile

-- | Removes the named image profile and returns it
removeImageProfile :: (MonadResource m) => Ptr MagickWand -> Text -> m (Vector Word8)
removeImageProfile = getProfile F.magickRemoveImageProfile

-- | Adds a named profile to the magick wand. If a profile with the same
-- name already exists, it is replaced. This method differs from the
-- `profileImage` method in that it does not apply any CMS color profiles.
setImageProfile :: (MonadResource m) => Ptr MagickWand -> Text -> Vector Word8 -> m ()
setImageProfile w name profile =
  withException_ w $ useAsCString (encodeUtf8 name) $
  \cstr -> V.unsafeWith profile $
           \p -> (F.magickSetImageProfile w cstr) p (fromIntegral $ V.length profile)

-- | Returns all the profile names that match the specified pattern
-- associated with a wand.
getImageProfiles :: (MonadResource m) => Ptr MagickWand -> Text -> m [Text]
getImageProfiles w pattern = liftIO $ alloca $ \pn -> do
  pprofileps <- useAsCString (encodeUtf8 pattern) (\cstr -> F.magickGetImageProfiles w cstr pn)
  n <- fromIntegral <$> peek pn
  profileps <- peekArray n pprofileps
  profiles <- forM profileps $ \profilep -> do
    profile <- decodeUtf8 <$> packCString profilep
    F.magickRelinquishMemory (castPtr profilep)
    return profile
  F.magickRelinquishMemory (castPtr pprofileps)
  return profiles
