{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/pixel_mod.htm
-- example/pixel_mod.c
-- Change the colour of one pixel in the logo: image
-- using either DrawPoint or a RegionIterator

import           Control.Applicative             ((<$>))
import           Data.Maybe
import           Data.Vector.Storable            ((!))
import           Graphics.ImageMagick.MagickWand

main :: IO ()
main = withMagickWandGenesis $ do
  {- Create a wand -}
  (_,mw) <- magickWand
  {- Read the input image -}
  readImage mw "logo:"
  -- Change this define to `False` to use the region iterator instead of the Draw
  let useDraw = True
  if useDraw
    then do
      -- Get a one-pixel region at coordinate 200,100
      (_,iterator) <- pixelRegionIterator mw 200 100 1 1
      pixels <- fromJust <$> pixelGetNextIteratorRow iterator
      -- Modify the pixel
      pixels!0 `setColor`  "red"
      -- then sync it back into the wand
      pixelSyncIterator iterator
    else do
      fill <- pixelWand
      (_,dw) <- drawingWand
      -- Set the fill to "red" or you can do the same thing with this:
      -- 	PixelSetColor(fill,"rgb(255,0,0)");
      fill `setColor` "red"
      dw `setFillColor` fill
      -- Uses the current Fill as the colour of the point at 200,100
      drawPoint dw 200 100
      {-
	srand(time(0));
	for(i=0;i<50;i++) {
		// plonk some random black pixels in the image
		j = rand()%DS_WIDTH;
		k = rand()%DS_HEIGHT;
		image[k*DS_WIDTH+j] = 0;
	}
      -}
      drawImage mw dw

  {- write it -}
  writeImage mw (Just "logo_pixel.gif")
