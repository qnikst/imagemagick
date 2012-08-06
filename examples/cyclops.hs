{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/cyclops.htm
-- http://www.imagemagick.org/discourse-server/viewtopic.php?f=18&t=12118
{-
magick convert cyclops.gif -bordercolor white -border 1x1 -matte \
		-fill none -fuzz 20% -draw "matte 0,0 floodfill" \
		-shave 1x1 cyclops_flood_3.png
-}

import           Filesystem.Path.CurrentOS                 (decodeString)
import           Graphics.ImageMagick.MagickWand
import           Graphics.ImageMagick.MagickWand.FFI.Types

main :: IO ()
main =
  withMagickWandGenesis $ do
    (_,w) <- magickWand
    readImage w (decodeString src)

    fc <- pixelWand
    bc <- pixelWand

    fc `setColor` "none"
    bc `setColor` "white"

    borderImage w bc 1 1
    setImageAlphaChannel w setAlphaChannel
    channel <- parseChannelOption "rgba"
    floodfillPaintImage w channel fc 20 bc 0 0 False
    shaveImage w 1 1
    writeImages w (decodeString out) True

  where
    src = "cyclops_sm.gif"
    out = "cyclops_sm_flood.png"
