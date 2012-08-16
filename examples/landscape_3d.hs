{-# LANGUAGE OverloadedStrings   #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/landscape_3d.htm
-- This is derived from a PHP script at:
-- http://eclecticdjs.com/mike/tutorials/php/imagemagick/imagickpixeliterator/3D_landscape.php

import           Control.Applicative             ((<$>))
import           Control.Monad
import           Control.Monad.IO.Class          (liftIO)
import           Data.IORef
import           Graphics.ImageMagick.MagickWand


main :: IO ()
main = withMagickWandGenesis $ do
  -- input image
  -- The input image is at: "http://eclecticdjs.com/mike/temp/ball/fract6.jpg"
  let url = "fract6.jpg";
  -- output image
  let file = "3d_fractal.jpg";

  (_,image) <- magickWand
  pw <- pixelWand

  readImage image url
  -- scale it down
  -- w <- getImageWidth image
  -- h <- getImageHeight image

  pw `setColor` "transparent"
  shearImage image pw 45 0
  --	MessageBox(NULL,"B - Shear failed","",MB_OK);
  w0 <- getImageWidth image
  h0 <- getImageHeight image

  -- scale it to make it look like it is laying down
  scaleImage image w0 (h0 `div` 2)
  --	MessageBox(NULL,"C - Scale failed","",MB_OK);
  -- Get image stats
  w <- getImageWidth image
  h <- getImageHeight image

  -- Make a blank canvas to draw on
  (_,canvas) <- magickWand
  -- Use a colour from the input image
  getImagePixelColor image 0 0 pw
  newImage canvas w (h*2) pw

  let offset = h
  -- The original drawing method was to go along each row from top to bottom so that
  -- a line in the "front" (which is one lower down the picture) will be drawn over
  -- one behind it.
  -- The problem with this method is that every line is drawn even if it will be covered
  -- up by a line "in front" of it later on.
  -- The method used here goes up each column from left to right and only draws a line if
  -- it is longer than everything drawn so far in this column and will therefore be visible.
  -- With the new drawing method this takes 13 secs - the previous method took 59 secs
  -- loop through all points in image
  forM_ [0..(w-1)] $ \x -> localGenesis $ do
    -- The PHP version created, used and destroyed the drawingwand inside
    -- the inner loop but it is about 25% faster to do only the DrawLine
    -- inside the loop
    (_,line) <- drawingWand
    line_height <- liftIO $ newIORef 0 -- let line_height = 0
    forM_ [(h-1),(h-2)..0] $ \y -> do
      -- get (r,g,b) and grey value
      getImagePixelColor image x y pw
      -- 255* adjusts the rgb values to Q8 even if the IM being used is Q16
      r <- round <$> (255*) <$> getRed pw
      g <- round <$> (255*) <$> getGreen pw
      b <- round <$> (255*) <$> getBlue pw

      -- Calculate grayscale - a divisor of 10-25 seems to work well.
      -- grey = (r+g+b)/25;
      let grey = (r + g + b) `div` 15
      -- grey = (r+g+b)/10;
      -- Only draw a line if it will show "above" what's already been done
      current <- liftIO $ readIORef line_height
      when (current == 0 || current < grey) $ do
        line `setFillColor` pw
        line `setStrokeColor` pw
        -- Draw the part of the line that is visible
        let
  	  start_y = y + offset - current
          end_y = y - grey + offset
        drawLine line (fromIntegral x) (fromIntegral start_y) (fromIntegral x) (fromIntegral end_y)
  	liftIO $ writeIORef line_height grey
      liftIO $ modifyIORef line_height (\n->n-1)

    -- Draw the lines on the image
    drawImage canvas line

  scaleImage canvas (w-h) (h*2)
  -- write canvas
  writeImage canvas (Just file)
