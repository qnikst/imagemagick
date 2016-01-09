{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/text_effects.htm
{- There's no equivalent convert command for this. It is a demo of MagickWand.
See this forum thread for the genesis of these effects
http://www.imagemagick.org/discourse-server/viewtopic.php?f=6&t=11586
and Anthony's Text Effects page at:
http://www.imagemagick.org/Usage/fonts/
-}

import           Control.Monad                         (when)
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Trans.Resource
import           Data.ByteString                       (ByteString)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand

-- see http://www.imagemagick.org/Usage/#font about using fonts with IM
font :: ByteString
font = "VerdanaBI"

-- Text effect 1 - shadow effect using MagickShadowImage
-- This is derived from Anthony's Soft Shadow effect
-- convert -size 300x100 xc:none -font Candice -pointsize 72 \
--           -fill white  -stroke black  -annotate +25+65 'Anthony' \
--           \( +clone -background navy  -shadow 70x4+5+5 \) +swap \
--           -background lightblue -flatten  -trim +repage  font_shadow_soft.jpg

-- NOTE - if an image has a transparent background, adding a border of any colour other
-- than "none" will remove all the transparency and replace it with the border's colour
textEffect1 :: (MonadResource m) => PMagickWand -> PDrawingWand -> PPixelWand -> m ()
textEffect1 w dw pw = do
  pw `setColor` "none"
  -- Create a new transparent image
  newImage w 350 100 pw
  -- Set up a 72 point white font
  pw `setColor` "white"
  dw `setFillColor` pw
  dw `setFont` font
  dw `setFontSize` 72
  -- Add a black outline to the text
  pw `setColor` "black"
  dw `setStrokeColor` pw
  -- Turn antialias on - not sure this makes a difference
  dw `setTextAntialias` True
  -- Now draw the text
  drawAnnotation dw 25 65 "Magick"
  -- Draw the image on to the magick_wand
  drawImage w dw

  -- Trim the image down to include only the text
  trimImage w 0
  -- equivalent to the command line +repage
  resetImagePage w Nothing

  -- Make a copy of the text image
  (_,cloneW) <- cloneMagickWand w
  -- Set the background colour to blue for the shadow
  pw `setColor` "blue"
  w `setImageBackgroundColor` pw

  -- Opacity is a real number indicating (apparently) percentage
  shadowImage w 70 4 5 5

  -- Composite the text on top of the shadow
  compositeImage w cloneW overCompositeOp 5 5

  (_,w') <- magickWand
  -- Create a new image the same size as the text image and put a solid colour
  -- as its background
  pw `setColor` "rgb(125,215,255)"
  width <- getImageWidth w
  height <- getImageHeight w
  newImage w' width height pw
  -- Now composite the shadowed text over the plain background
  compositeImage w' w overCompositeOp 0 0
  --  and write the result
  writeImage w' (Just "text_shadow.png")


-- Given a pattern name (which MUST have a leading #) and a pattern file,
-- set up a pattern URL for later reference in the specified drawing wand
-- Currently only used in Text Effect 2
setTilePattern :: (MonadResource m) => PDrawingWand -> Text -> FilePath -> m ()
setTilePattern dw patternName patternFile = do
  (_,w) <- magickWand
  readImage w (T.pack patternFile)
  -- Read the tile's width and height
  width <- getImageWidth w
  height <- getImageHeight w

  pushPattern dw (T.tail patternName) 0 0 (realToFrac width) (realToFrac height)
  drawComposite dw srcOverCompositeOp 0 0 0 0 w
  popPattern dw
  dw `setFillPatternURL` patternName


-- Text effect 2 - tiled text using the builtin checkerboard pattern
-- Anthony's Tiled Font effect
-- convert -size 320x100 xc:lightblue -font Candice -pointsize 72 \
--          -tile pattern:checkerboard   -annotate +28+68 'Anthony' \
--           font_tile.jpg
textEffect2 :: (MonadResource m) => PMagickWand -> PDrawingWand -> PPixelWand -> m ()
textEffect2 w dw pw = do
  setTilePattern dw "#check" "pattern:checkerboard"

  pw `setColor` "lightblue"
  -- Create a new transparent image
  newImage w 320 100 pw

  -- Set up a 72 point font
  dw `setFont` font
  dw `setFontSize` 72
  -- Now draw the text
  drawAnnotation dw 28 68 "Magick"
  -- Draw the image on to the magick_wand
  drawImage w dw
  -- Trim the image
  trimImage w 0
  -- Add a transparent border
  pw `setColor` "lightblue"
  borderImage w pw 5 5
  -- and write it
  writeImage w (Just "text_pattern.png")

-- Text effect 3 -  arc font (similar to http://www.imagemagick.org/Usage/fonts/#arc)
-- convert -size 320x100 xc:lightblue -font Candice -pointsize 72 \
--           -annotate +25+65 'Anthony' -distort Arc 120 \
--           -trim +repage -bordercolor lightblue -border 10  font_arc.jpg
textEffect3 :: (MonadResource m) => PMagickWand -> PDrawingWand -> PPixelWand -> m ()
textEffect3 w dw pw = do
  -- Create a 320x100 lightblue canvas
  pw `setColor` "lightblue"
  newImage w 320 100  pw

  -- Set up a 72 point font
  dw `setFont` font
  dw `setFontSize` 72
  -- Now draw the text
  drawAnnotation dw 25 65 "Magick"
  -- Draw the image on to the magick_wand
  drawImage w dw

  let dargs = [120]
  distortImage w arcDistortion dargs False
  -- Trim the image
  trimImage w 0
  -- Add the border
  pw `setColor` "lightblue"
  borderImage w pw 10 10

  -- and write it
  writeImage w (Just "text_arc.png")


-- Text effect 4 - bevelled font http://www.imagemagick.org/Usage/fonts/#bevel
-- convert -size 320x100 xc:black -font Candice -pointsize 72 \
--              -fill white   -annotate +25+65 'Anthony' \
--              -shade 140x60  font_beveled.jpg
textEffect4 :: (MonadResource m) => PMagickWand -> PDrawingWand -> PPixelWand -> m ()
textEffect4 w dw pw = do
  let colorize = False
  -- Create a 320x100 canvas
  pw `setColor` "gray"
  newImage w 320 100 pw
  -- Set up a 72 point font
  dw `setFont` font
  dw `setFontSize` 72
  -- Set up a 72 point white font
  pw `setColor` "white"
  dw `setFillColor` pw
  -- Now draw the text
  drawAnnotation dw 25 65 "Magick"
  -- Draw the image on to the magick_wand
  drawImage w dw
  -- the "gray" parameter must be true to get the effect shown on Anthony's page
  shadeImage w True 140 60

  when colorize $ do
    pw `setColor` "yellow"
    dw `setFillColor` pw
    pw' <- pixelWand
    pw' `setColor` "gold"
    colorizeImage w pw pw'

  -- and write it
  writeImage w (Just "text_bevel.png")


-- Text effect 5 and 6 - Plain text and then Barrel distortion
textEffects5_6 :: (MonadResource m) => PMagickWand -> PDrawingWand -> PPixelWand -> m ()
textEffects5_6 w dw pw = do
  -- Create a 320x100 transparent canvas
  pw `setColor` "none"
  newImage w 320 100 pw

  -- Set up a 72 point font
  dw `setFont` font
  dw `setFontSize` 72
  -- Now draw the text
  drawAnnotation dw 25 65 "Magick"
  -- Draw the image on to the magick_wand
  drawImage w dw
  writeImage w (Just"text_plain.png")

  -- Trim the image
  trimImage w 0
  -- Add the border
  pw `setColor` "none"
  borderImage w pw 10 10
  -- MagickSetImageMatte(magick_wand,MagickTrue);
  -- MagickSetImageVirtualPixelMethod(magick_wand,TransparentVirtualPixelMethod);
  -- d_args[0] = 0.1;d_args[1] = -0.25;d_args[2] = -0.25; [3] += .1
  -- The first value should be positive. If it is negative the image is *really* distorted
  -- d_args[0] = 0.0;
  -- d_args[1] = 0.0;
  -- d_args[2] = 0.5;
  -- d_args[3] should normally be chosen such the sum of all 4 values is 1
  -- so that the result is the same size as the original
  -- You can override the sum with a different value
  -- If the sum is greater than 1 the resulting image will be smaller than the original
  -- d_args[3] = 1 - (d_args[0] + d_args[1] + d_args[2]);
  -- Make the result image smaller so that it isn't as likely
  -- to overflow the edges
  -- d_args[3] += 0.1;
  -- 0.0,0.0,0.5,0.5,0.0,0.0,-0.5,1.9
  -- d_args[3] = 0.5;
  -- d_args[4] = 0.0;
  -- d_args[5] = 0.0;
  -- d_args[6] = -0.5;
  -- d_args[7] = 1.9;

  let d_args = [0, 0, 0.5, 1 - (0 + 0 + 0.5), 0, 0, -0.5, 1.9]
  -- DON'T FORGET to set the correct number of arguments here
  distortImage w barrelDistortion d_args True
  -- MagickResetImagePage(magick_wand,"");
  -- Trim the image again
  trimImage w 0
  -- Add the border
  pw `setColor` "none"
  borderImage w pw 10 10
  -- and write it
  writeImage w (Just "text_barrel.png")

-- Text effect 7 - Polar distortion
textEffect7 :: (MonadResource m) => PMagickWand -> PDrawingWand -> PPixelWand -> m ()
textEffect7 w dw pw = do
  -- Create a 320x200 transparent canvas
  pw `setColor` "none"
  newImage w 320 200 pw

  -- Set up a 72 point font
  dw `setFont` font
  dw `setFontSize` 72
  -- Now draw the text
  drawAnnotation dw 25 65 "Magick"
  -- Draw the image on to the magick_wand
  drawImage w dw

  distortImage w polarDistortion [0] True
  -- MagickResetImagePage(magick_wand,"");
  -- Trim the image again
  trimImage w 0
  -- Add the border
  pw `setColor` "none"
  borderImage w pw 10 10
  -- and write it
  writeImage w (Just "text_polar.png")

-- Text effect 8 - Shepard's distortion
textEffect8 :: (MonadResource m) => PMagickWand -> PDrawingWand -> PPixelWand -> m ()
textEffect8 w dw pw = do
  -- Create a 320x200 transparent canvas
  pw `setColor` "none"
  newImage w 640 480 pw

  -- Set up a 72 point font
  dw `setFont` font
  dw `setFontSize` 72
  -- Now draw the text
  drawAnnotation dw 50 240 "Magick Rocks"
  -- Draw the image on to the magick_wand
  drawImage w dw
  let d_args = [ 150.0, 190.0, 100.0, 290.0, 500.0, 200.0, 430.0, 130.0 ]
  distortImage w shepardsDistortion d_args True

  -- Trim the image
  trimImage w 0
  -- Add the border
  pw `setColor` "none"
  borderImage w pw 10 10
  -- and write it
  writeImage w (Just "text_shepards.png")

runEffect :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
             (PMagickWand -> PDrawingWand -> PPixelWand -> ResourceT m ()) -> m ()
runEffect e = localGenesis $ do
  (_,w) <- magickWand
  (_,dw) <- drawingWand
  pw <- pixelWand
  e w dw pw

main :: IO ()
main = withMagickWandGenesis $ do
  runEffect textEffect1
  runEffect textEffect2
  runEffect textEffect3
  runEffect textEffect4
  runEffect textEffects5_6
  runEffect textEffect7
  runEffect textEffect8
