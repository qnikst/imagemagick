{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/floodfill.htm
--
-- Replace the white background area of 1st argument with transparent and don't forget
-- that for this the channel must be "rgba" and the output image must be PNG
-- or other format which supports transparency

import           Graphics.ImageMagick.MagickWand

main = do
  withMagickWandGenesis $ do
    (_,w) <- magickWand
    readImage w "logo:"

    fc <- pixelWand
    bc <- pixelWand

    fc `setColor` "none"
    bc `setColor` "white"

    channel <- parseChannelOption "rgba"

    floodfillPaintImage w channel fc 20 bc 0 0 False

    w `writeImage` (Just "logo_floodfill.png") 
