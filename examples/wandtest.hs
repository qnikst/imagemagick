{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                             (forM_, when)
import           Control.Monad.IO.Class                    (liftIO)
import           Control.Monad.Trans.Resource              (release)
import qualified Data.Text                                 as T
import           Data.Vector.Storable                      (Vector, (!))
import qualified Data.Vector.Storable                      as V
import           Data.Word
import           Graphics.ImageMagick.MagickWand
import           System.Exit
import           Text.Printf                               (printf)

exitWithMessage msg = liftIO $ do
  putStrLn msg
  exitFailure

iterateWand magick_wand = magickIterate magick_wand $ \w -> do
  i <- getIteratorIndex w
  s <- getImageScene w
  liftIO $ putStrLn $ printf "index %2d scene %2d" i s


main :: IO ()
main = withMagickWandGenesis $ do
  (_,magick_wand) <- magickWand
  setSize magick_wand 640 480
  size <- getSize magick_wand
  when (size /= (640,480)) $ exitWithMessage "Unexpected magick wand size"
  liftIO $ putStrLn "Reading images...\n"
  readImage magick_wand "sequence.miff"
  n <- getNumberImages magick_wand
  when (n /= 5) $ liftIO $ putStrLn $ printf "read %02d images; expected 5" n
  liftIO $ putStrLn "Iterate forward..."
  iterateWand magick_wand

  liftIO $ putStrLn "Iterate reverse..."
  magickIterateReverse magick_wand $ \w -> do
    i <- getIteratorIndex w
    s <- getImageScene w
    liftIO $ putStrLn $ printf "index %2d scene %2d" i s

  liftIO $ putStrLn "Remove scene 1..."
  setIteratorIndex magick_wand 1
  (clone_key,clone_wand) <- getImage magick_wand
  removeImage magick_wand
  iterateWand magick_wand

  liftIO $ putStrLn "Insert scene 1 back in sequence..."
  setIteratorIndex magick_wand 0
  addImage magick_wand clone_wand
  iterateWand magick_wand

  liftIO $ putStrLn "Set scene 2 to scene 1..."
  setIteratorIndex magick_wand 2
  setImage magick_wand clone_wand
  release clone_key
  iterateWand magick_wand

  liftIO $ putStrLn "Apply image processing options..."
  cropImage magick_wand 60 60 10 10
  resetIterator magick_wand
  background <- pixelWand
  background `setColor` "#000000"
  rotateImage magick_wand background 90.0
  border <- pixelWand
  background `setColor` "green"
  border `setColor` "black"
  floodfillPaintImage magick_wand compositeChannels background
    (0.01*quantumRange) border 0 0 False

  (drawing_key,drawing_wand) <- drawingWand
  pushDrawingWand drawing_wand
  rotate drawing_wand 45
  drawing_wand `setFontSize` 18
  fill <- pixelWand
  fill `setColor` "green"
  drawing_wand `setFillColor` fill
  -- ? fill=DestroyPixelWand(fill);
  drawAnnotation drawing_wand 15 5 "Magick"
  popDrawingWand drawing_wand
  setIteratorIndex magick_wand 1
  drawImage magick_wand drawing_wand
  annotateImage magick_wand drawing_wand 70 5 90 "Image"
  release drawing_key

  let primary_colors = [
          0,   0,   0,
          0,   0, 255,
          0, 255,   0,
          0, 255, 255,
        255, 255, 255,
        255,   0,   0,
        255,   0, 255,
        255, 255,   0,
        128, 128, 128
        ] :: [Word8]

  setIteratorIndex magick_wand 2
  importImagePixels magick_wand 10 10 3 3 "RGB" primary_colors
  pixels <- exportImagePixels magick_wand 10 10 3 3 "RGB"
  when (pixels /= primary_colors) $ exitWithMessage "Get pixels does not match set pixels"

  setIteratorIndex magick_wand 3
  resizeImage magick_wand 50 50 undefinedFilter 1.0
  magickIterate magick_wand $ \w -> do
    setImageDepth w 8
    setImageCompression w rleCompression

  resetIterator magick_wand
  setIteratorIndex magick_wand 4
  liftIO $ putStrLn "Utilitize pixel iterator to draw diagonal..."
  (iterator_key,iterator) <- pixelIterator magick_wand
  pixelRows <- pixelIterateList iterator
  forM_ (zip [0..] pixelRows) $ \(i, pixelRow) -> do
    pixelRow!i `setColor` "#224466"
    pixelSyncIterator iterator
  release iterator_key

  liftIO $ putStrLn "Write to wandtest_out.miff..."
  writeImages magick_wand "wandtest_out.miff" True
  liftIO $ putStrLn "Change image format from \"MIFF\" to \"GIF\"..."
  setImageFormat magick_wand "GIF"
  let wandDelay = 3
      newDelay = 100 * wandDelay
  liftIO $ putStrLn $ printf "Set delay between frames to %d seconds..." wandDelay
  setImageDelay magick_wand newDelay
  delay <- getImageDelay magick_wand
  when (delay /= newDelay) $ exitWithMessage "Get delay does not match set delay"
  liftIO $ putStrLn "Write to wandtest_out.gif..."
  writeImages magick_wand "wandtest_out.gif" True

  let customOption = "custom option"
      customOptionName = "wand:custom-option"
  liftIO $ putStrLn "Set, list, get, and delete wand option..."
  setOption magick_wand customOptionName customOption
  option <- getOption magick_wand customOptionName
  when (option /= customOption) $ exitWithMessage "Option does not match"
  options <- getOptions magick_wand "*"
  forM_ options $ \o -> liftIO $ putStrLn $ printf "  %s" (T.unpack o)
  deleteOption magick_wand customOptionName

  let customPropertyName = "wand:custom-property"
      customProperty = "custom profile"
  liftIO $ putStrLn "Set, list, get, and delete wand property..."
  setImageProperty magick_wand customPropertyName customProperty
  property <- getImageProperty magick_wand customPropertyName
  when (property /= customProperty) $ exitWithMessage "Property does not match"
  properties <- getImageProperties magick_wand "*"
  forM_ properties $ \p -> liftIO $ putStrLn $ printf "  %s" (T.unpack p)
  deleteImageProperty magick_wand customPropertyName

  let profileName = "sRGB"
  liftIO $ putStrLn "Set, list, get, and remove sRGB color profile..."
  setImageProfile magick_wand profileName sRGBProfile
  profile <- getImageProfile magick_wand profileName
  when (profile /= sRGBProfile) $ exitWithMessage "Profile does not match"
  profiles <- getImageProfiles magick_wand "*"
  forM_ profiles $ \p -> liftIO $ putStrLn $ printf "  %s" (T.unpack p)
  removedProfile <- removeImageProfile magick_wand profileName
  when (removedProfile /= sRGBProfile) $ exitWithMessage "Profile does not match"
  liftIO $ putStrLn "Wand tests pass."

-- only first 24 bytes from wandtest.c taken (no actual need for 60k profile)
sRGBProfile :: Vector Word8
sRGBProfile = V.fromList [
  0x00, 0x00, 0xee, 0x20, 0x00, 0x00, 0x00, 0x00, 0x04, 0x20, 0x00, 0x00,
  0x73, 0x70, 0x61, 0x63, 0x52, 0x47, 0x42, 0x20, 0x4c, 0x61, 0x62, 0x20
  ]
