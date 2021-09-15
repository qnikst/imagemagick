{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main (main) where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Resource
import           Control.Monad.Catch
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as BS
import           Data.Maybe
import qualified Data.Vector.Storable            as V
import qualified Data.Text as T
import           Data.Word
import           Prelude
import           System.Directory                (getTemporaryDirectory,
                                                  removeFile)
import           System.IO                       (hClose, openTempFile)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Graphics.ImageMagick.MagickWand
import Data.Text (pack)
import Graphics.ImageMagick.MagickWand.FFI.Types (undefinedOrientation, topLeftOrientation, topRightOrientation, bottomRightOrientation, bottomLeftOrientation, leftTopOrientation, rightTopOrientation, rightBottomOrientation, leftBottomOrientation)


main :: IO ()
main = defaultMain tests

-- tests mostly taken from wand(http://dahlia.kr/wand/) source code
tests :: TestTree
tests = localOption (mkTimeout 1000000) $ testGroup "Behaves to spec"
  [ testCase "reading file" test_readImage
  , testCase "getting orientation" test_getOrientation
  , testCase "auto orienting image" test_autoOrientImage
  , testCase "setting image orientation" test_setImageOrientation
  -- , testCase "stripping" test_strip
  -- , testCase "trimming" test_trim
  -- , testCase "format to MIME conversion" test_mime
  -- , testCase "iterate" test_iterate
  -- , testCase "getitng pixel" test_pixel
  -- , testCase "cropping image" test_crop
  -- , testCase "resizing image" test_resize
  -- , testCase "rotating image" test_rotate
  -- , testCase "image signature" test_signature
  -- , testCase "getting alpha channel" test_getImageAlphaChannel
  -- , testCase "setting alpha channel" test_setImageAlphaChannel
  -- , testCase "unsetting alpha channel" test_unsetImageAlphaChannel
  -- , testCase "getting background color" test_getImageBackgroundColor
  -- , testCase "setting background color" test_setImageBackgroundColor
  -- , testCase "watermark" test_watermark
  -- -- Creates black background instead of transparent
  -- -- , testCase "reset" test_reset
  -- , testCase "getting an imageS blob for an animated GIF" test_getImagesBlobForSequence
  -- , testCase "getting an imageS blob for a single image" test_getImagesBlobForSingle
  ]


test_readImage :: IO ()
test_readImage = withImage "mona-lisa.jpg" $ \w -> do
  width <- getImageWidth w
  liftIO $ width @?= 402

test_readImageBlob :: IO ()
test_readImageBlob = withWand $ \w -> do
  blob <- liftIO $ BS.readFile (dataFile "mona-lisa.jpg")
  readImageBlob w blob
  width <- getImageWidth w
  liftIO $ width @?= 402

test_cloneWand :: IO ()
test_cloneWand = withImage "mona-lisa.jpg" $ \w -> do
  size <- getImageSize w
  (_,w') <- cloneMagickWand w
  size' <- getImageSize w'
  liftIO $ size' @?= size

test_writeImage :: IO ()
test_writeImage = withImage "mona-lisa.jpg" $ \w -> do
  size <- getImageSize w
  d <- liftIO $ getTemporaryDirectory
  (tmpName, hTemp) <- liftIO $ openTempFile d ""
  liftIO $ hClose  hTemp
  writeImage w (Just $ T.pack tmpName)
  (_,w') <- magickWand
  readImage w' (T.pack tmpName)
  size' <- getImageSize w'
  liftIO $ removeFile tmpName
  liftIO $ size' @?= size

test_getReadImageBlob :: IO ()
test_getReadImageBlob = withImage "mona-lisa.jpg" $ \w -> do
  size <- getImageSize w
  blob <- getImageBlob w
  (_,w') <- magickWand
  readImageBlob w' blob
  size' <- getImageSize w'
  liftIO $ size' @?= size

test_size :: IO ()
test_size = withImage "mona-lisa.jpg" $ \w -> do
  size <- getImageSize w
  liftIO $ size @?= (402,599)

test_getDepth :: IO ()
test_getDepth = withImage "mona-lisa.jpg" $ \w -> do
  depth <- getImageDepth w
  liftIO $ depth @?= 8

test_setDepth :: IO ()
test_setDepth = withImage "mona-lisa.jpg" $ \w -> do
  let newDepth = 16
  setImageDepth w newDepth
  depth <- getImageDepth w
  liftIO $ depth @?= newDepth

test_formatJpeg :: IO ()
test_formatJpeg = withImage "mona-lisa.jpg" $ \w -> do
  format <- getImageFormat w
  liftIO $ format @?= "JPEG"

test_formatPng :: IO ()
test_formatPng = withImage "croptest.png" $ \w -> do
  format <- getImageFormat w
  liftIO $ format @?= "PNG"

test_setFormat :: IO ()
test_setFormat = withImage "mona-lisa.jpg" $ \w -> do
  setImageFormat w "PNG"
  format <- getImageFormat w
  liftIO $ format @?= "PNG"
  bs <- getImageBlob w
  (_,w') <- magickWand
  readImageBlob w' bs
  format' <- getImageFormat w'
  liftIO $ format' @?= format

test_setBadFormat :: IO ()
test_setBadFormat = withImage "mona-lisa.jpg" $
                    \w -> assertMagickWandException $ setImageFormat w "HANKY"

test_getCompressionQuality :: IO ()
test_getCompressionQuality = withImage "mona-lisa.jpg" $ \w -> do
  quality <- getImageCompressionQuality w
  liftIO $ quality @?= 80

test_setCompressionQuality :: IO ()
test_setCompressionQuality = withImage "mona-lisa.jpg" $ \w -> do
  let quality = 50
  setImageCompressionQuality w quality
  quality' <- getImageCompressionQuality w
  liftIO $ quality' @?= quality
  bs <- getImageBlob w
  (_,w') <- magickWand
  readImageBlob w' bs
  quality'' <- getImageCompressionQuality w'
  liftIO $ quality'' @?= quality

test_strip :: IO ()
test_strip = withImage "beach.jpg"$ \w -> do
  originalLength <- BS.length <$> getImageBlob w
  stripImage w
  strippedLength <- BS.length <$> getImageBlob w
  liftIO $ assertBool "stripped should be shorter than original" $ strippedLength < originalLength

test_trim :: IO ()
test_trim = withImage "trimtest.png" $ \w -> do
  (width,height) <- getImageSize w
  trimImage w fuzz
  (width',height') <- getImageSize w
  liftIO $ assertBool "width should be trimmed" $ width' < width
  liftIO $ assertBool "height should be trimmed" $ height' < height

test_mime :: IO ()
test_mime = withMagickWandGenesis $ do
  mime <- toMime "JPG"
  liftIO $ assertBool "jpg should convert to either image/jpeg or image/x-jpeg" $
    mime `elem` ["image/jpeg", "image/x-jpeg"]
  mime' <- toMime "PNG"
  liftIO $ assertBool "jpg should convert to either image/png or image/x-png" $
    mime' `elem` ["image/png", "image/x-png"]

test_iterate :: IO ()
test_iterate = withImage "croptest.png" $ \w -> do
  black <- getColorPW "#000"
  transparent <- getColorPW "transparent"
  (_,it) <- pixelIterator w
  rows <- pixelIterateList it
  forM_ (zip [0..] rows) $ \(i, row) -> do
    when (i `mod` 3 == 0) $ do -- condition to make test a bit faster
      let rowLength = V.length row
      liftIO $ rowLength @?= 300
      if i >= 100 && i < 200
        then do
          let
            assertPixel j pw | j >= 100 && j < 200 = assertEqualPW black pw
            assertPixel _ pw = assertEqualPW transparent pw
          V.zipWithM_ assertPixel (V.enumFromN (0::Word16) rowLength) row
        else do
          V.forM_ row $ assertEqualPW transparent
  liftIO $ (length rows) @?= 300

test_pixel :: IO ()
test_pixel  = withImage "croptest.png" $ \w -> do
  transparent <- getColorPW "transparent"
  black <- getColorPW "#000"
  pw <- pixelWand
  getImagePixelColor w 0 0 pw
  assertEqualPW pw transparent
  getImagePixelColor w 99 99 pw
  assertEqualPW pw transparent
  getImagePixelColor w 100 100 pw
  assertEqualPW pw black
  getImagePixelColor w 150 150 pw
  assertEqualPW pw black
  getImagePixelColor w 201 201 pw
  assertEqualPW pw transparent

test_crop :: IO ()
test_crop = withImage "croptest.png" $ \w -> do
  black <- getColorPW "#000"
  cropImage w 100 100 100 100
  size <- getImageSize w
  liftIO $ size @?= (100,100)
  (_,it) <- pixelIterator w
  rows <- pixelIterateList it
  forM_ rows $ \row -> V.forM_ row (assertEqualPW black)

test_resize :: IO ()
test_resize = withImage "mona-lisa.jpg" $ \w -> do
  size <- getImageSize w
  liftIO $ size @?= (402,599)
  resizeImage w 100 100 undefinedFilter 1
  size' <- getImageSize w
  liftIO $ size' @?= (100,100)

test_rotate :: IO ()
test_rotate = withImage "rotatetest.gif" $ \w -> do
  transparent <- getColorPW "transparent"
  black <- getColorPW "black"
  white <- getColorPW "white"
  red <- getColorPW "red"
  size <- getImageSize w
  liftIO $ size @?= (150,100)
  localGenesis $ do
    (_,w') <- cloneMagickWand w
    rotateImage w' transparent 360
    size' <- getImageSize w'
    liftIO $ size' @?= size
    assertColorAtXY w' black ( 0,50)
    assertColorAtXY w' black (74,50)
    assertColorAtXY w' black ( 0,99)
    assertColorAtXY w' black (74,99)
    assertColorAtXY w' white (75,50)
    assertColorAtXY w' white (75,99)
  localGenesis $ do
    (_,w') <- cloneMagickWand w
    rotateImage w' transparent 90
    size' <- getImageSize w'
    liftIO $ size' @?= (100,150)
    (_,it) <- pixelIterator w'
    rows <- pixelIterateList it
    forM_ (zip [0..] rows) $ \(y,row) -> do
      let
        assertPixel x pw | x < 50 && y < 75 = assertEqualPW black pw
        assertPixel _ pw = assertEqualPW white pw
      V.zipWithM_ assertPixel (V.enumFromN (0::Word16) (V.length row)) row
  localGenesis $ do
    (_,w') <- cloneMagickWand w
    rotateImage w' red 45
    (width', height') <- getImageSize w'
    liftIO $ assertBool "size should be (>=176)x(<=178)" $
      width' >= 176 && height' <= 178
    assertColorAtXY w' red (       0,         0)
    assertColorAtXY w' red (       0, height'-1)
    assertColorAtXY w' red (width'-1,         0)
    assertColorAtXY w' red (width'-1, height'-1)
    assertColorAtXY w' black ( 2, 70)
    assertColorAtXY w' black (35, 37)
    assertColorAtXY w' black (85, 88)
    assertColorAtXY w' black (52,120)

test_signature :: IO ()
test_signature = withImage "mona-lisa.jpg" $ \w -> do
  let signature = "f7695e173f691f59c5939e1898eafa6491bdf1439c60ecce7edfe4b3d101bf22"
  testedSignature <- getImageSignature w
  liftIO $ testedSignature @?= signature

test_getImageAlphaChannel :: IO ()
test_getImageAlphaChannel = do
  withImage "watermark.png" $ \w -> do
    alphaCh <- getImageAlphaChannel w
    liftIO $ alphaCh @?= True
  withImage "mona-lisa.jpg" $ \w -> do
    alphaCh <- getImageAlphaChannel w
    liftIO $ alphaCh @?= False

test_setImageAlphaChannel :: IO ()
test_setImageAlphaChannel = withImage "mona-lisa.jpg" $ \w -> do
  setImageAlphaChannel w deactivateAlphaChannel
  alphaCh <- getImageAlphaChannel w
  liftIO $ alphaCh @?= False
  setImageAlphaChannel w activateAlphaChannel
  alphaCh' <- getImageAlphaChannel w
  liftIO $ alphaCh' @?= True

test_unsetImageAlphaChannel :: IO ()
test_unsetImageAlphaChannel = withImage "watermark.png" $ \w -> do
  alphaCh <- getImageAlphaChannel w
  liftIO $ alphaCh @?= True
  setImageAlphaChannel w deactivateAlphaChannel
  alphaCh' <- getImageAlphaChannel w
  liftIO $ alphaCh' @?= False

test_getImageBackgroundColor :: IO ()
test_getImageBackgroundColor = withImage "mona-lisa.jpg" $ \w -> do
  white <- getColorPW "white"
  bg <- getImageBackgroundColor w
  assertEqualPW bg white

test_setImageBackgroundColor :: IO ()
test_setImageBackgroundColor = withImage "croptest.png" $ \w -> do
  transparent <- getColorPW "transparent"
  setImageBackgroundColor w transparent
  bg <- getImageBackgroundColor w
  assertEqualPW bg transparent

test_watermark :: IO ()
test_watermark = withImage "beach.jpg" $ \w -> do
  (_,watermark) <- magickWand
  readImage watermark (T.pack $ dataFile "watermark.png")
  setIteratorIndex watermark 0
  setImageType watermark trueColorMatteType
  evaluateImageChannel watermark opacityChannel subtractEvaluateOperator (0.3 * quantumRange)
  compositeImage w watermark overCompositeOp 0 0
  (_,marked) <- magickWand
  readImage marked (T.pack $ dataFile "marked.png")
  sig1 <- getImageSignature marked
  sig2 <- getImageSignature w
  liftIO $ sig2 @?= sig1

test_reset :: IO ()
test_reset = withImage "sasha.jpg" $ \w -> do
  transparent <- getColorPW "transparent"
  rotateImage w transparent 45
  resetImagePage w Nothing
  cropImage w 170 170 0 0
  resetImagePage w Nothing
  sig1 <- getImageSignature w
  (_,control) <- magickWand
  readImage control (T.pack $ dataFile "resettest.png")
  sig2 <- getImageSignature control
  liftIO $ sig1 @?= sig2

test_getOrientation :: IO ()
test_getOrientation = withWand $ \w -> do
  readImage w (pack. dataFile $ "orientation_0.jpeg")
  orientation_0 <- getImageOrientation w
  liftIO $ orientation_0 @?= undefinedOrientation
  readImage w (pack. dataFile $ "orientation_1.jpeg")
  orientation_1 <- getImageOrientation w
  liftIO $ orientation_1 @?= topLeftOrientation
  readImage w (pack. dataFile $ "orientation_2.jpeg")
  orientation_2 <- getImageOrientation w
  liftIO $ orientation_2 @?= topRightOrientation
  readImage w (pack. dataFile $ "orientation_3.jpeg")
  orientation_3 <- getImageOrientation w
  liftIO $ orientation_3 @?= bottomRightOrientation
  readImage w (pack. dataFile $ "orientation_4.jpeg")
  orientation_4 <- getImageOrientation w
  liftIO $ orientation_4 @?= bottomLeftOrientation
  readImage w (pack. dataFile $ "orientation_5.jpeg")
  orientation_5 <- getImageOrientation w
  liftIO $ orientation_5 @?= leftTopOrientation
  readImage w (pack. dataFile $ "orientation_6.jpeg")
  orientation_6 <- getImageOrientation w
  liftIO $ orientation_6 @?= rightTopOrientation
  readImage w (pack. dataFile $ "orientation_7.jpeg")
  orientation_7 <- getImageOrientation w
  liftIO $ orientation_7 @?= rightBottomOrientation
  readImage w (pack. dataFile $ "orientation_8.jpeg")
  orientation_8 <- getImageOrientation w
  liftIO $ orientation_8 @?= leftBottomOrientation

test_autoOrientImage :: IO ()
test_autoOrientImage = withWand $ \w -> do
  readImage w (pack. dataFile $ "orientation_0.jpeg")
  autoOrientImage w
  orientation_0 <- getImageOrientation w
  liftIO $ orientation_0 @?= topLeftOrientation
  readImage w (pack. dataFile $ "orientation_1.jpeg")
  autoOrientImage w
  orientation_1 <- getImageOrientation w
  liftIO $ orientation_1 @?= topLeftOrientation
  readImage w (pack. dataFile $ "orientation_2.jpeg")
  autoOrientImage w
  orientation_2 <- getImageOrientation w
  liftIO $ orientation_2 @?= topLeftOrientation
  readImage w (pack. dataFile $ "orientation_3.jpeg")
  autoOrientImage w
  orientation_3 <- getImageOrientation w
  liftIO $ orientation_3 @?= topLeftOrientation
  readImage w (pack. dataFile $ "orientation_4.jpeg")
  autoOrientImage w
  orientation_4 <- getImageOrientation w
  liftIO $ orientation_4 @?= topLeftOrientation
  readImage w (pack. dataFile $ "orientation_5.jpeg")
  autoOrientImage w
  orientation_5 <- getImageOrientation w
  liftIO $ orientation_5 @?= topLeftOrientation
  readImage w (pack. dataFile $ "orientation_6.jpeg")
  autoOrientImage w
  orientation_6 <- getImageOrientation w
  liftIO $ orientation_6 @?= topLeftOrientation
  readImage w (pack. dataFile $ "orientation_7.jpeg")
  autoOrientImage w
  orientation_7 <- getImageOrientation w
  liftIO $ orientation_7 @?= topLeftOrientation
  readImage w (pack. dataFile $ "orientation_8.jpeg")
  autoOrientImage w
  orientation_8 <- getImageOrientation w
  liftIO $ orientation_8 @?= topLeftOrientation

test_setImageOrientation :: IO ()
test_setImageOrientation = withImage "orientation_0.jpeg" $ \w -> do
  setImageOrientation w topLeftOrientation
  orientation <- getImageOrientation w
  liftIO $ orientation @?= topLeftOrientation

-- helpers

fuzz :: Double
fuzz = 10

dataFile :: String -> FilePath
dataFile name = "data/" ++ name

withWand :: (PMagickWand -> ResourceT IO a) -> IO a
withWand f =  withMagickWandGenesis $ do
  (_,w) <- magickWand
  f w

withImage :: String -> (PMagickWand -> ResourceT IO a) -> IO a
withImage name f = withWand $ \w -> do
  readImage w (T.pack $ dataFile name)
  f w

getImageSize :: (MonadResource m) => PMagickWand -> m (Int, Int)
getImageSize w = do
  width <- getImageWidth w
  height <- getImageHeight w
  return (width,height)

-- TODO fix to Text
getColorPW :: (MonadResource m) => ByteString -> m PPixelWand
getColorPW color = do
  pw <- pixelWand
  pw `setColor` color
  return pw

assertMagickWandException :: (MonadResource m, MonadCatch m) => m a -> m ()
assertMagickWandException action =
  catch (action >> (liftIO $ assertFailure "Expected MagickWandException"))
        (\(_::MagickWandException) -> return ())

assertEqualPW :: (MonadResource m) => PPixelWand -> PPixelWand -> m ()
assertEqualPW pw1 pw2 = do
  a1 <- getAlpha pw1
  a2 <- getAlpha pw2
  similar <- isPixelWandSimilar pw1 pw2 fuzz
--  let x = show (a1,a2,similar)
  liftIO $ assertBool "colors should be similar" $ (a1 == a2) && similar

assertColorAtXY :: (MonadResource m) => PMagickWand -> PPixelWand -> (Int,Int) -> m ()
assertColorAtXY w pw1 (x,y) = do
  pw2 <- pixelWand
  getImagePixelColor w x y pw2
  assertEqualPW pw1 pw2

test_getImagesBlobForSequence :: IO()
test_getImagesBlobForSequence = testGetImagesBlob "newtons-cradle.gif"

test_getImagesBlobForSingle :: IO()
test_getImagesBlobForSingle = testGetImagesBlob "mona-lisa.jpg"

testGetImagesBlob :: String -> IO()
testGetImagesBlob imgName = withImage imgName $ \w -> do
  numImgs <- getNumberImages w
  size <- getImageSize w
  blob <- getImagesBlob w
  (_,w') <- magickWand
  readImageBlob w' blob
  numImgs' <- getNumberImages w'
  size' <- getImageSize w'
  liftIO $ numImgs @?= numImgs'
  liftIO $ size' @?= size
