{-# LANGUAGE OverloadedStrings #-}

-- http://members.shaw.ca/el.supremo/MagickWand/extent.htm
-- convert logo: -background blue -extent 1024x768-192-144 logo_extent.jpg
-- Read an image and centre it on a larger 1024x768, extended canvas.
-- The input image must be no larger than 1024x768 because this code does not
-- check for errors

import           Graphics.ImageMagick.MagickWand

main :: IO ()
main = do

  withMagickWandGenesis $ do
    (_,w) <- magickWand
    p <- pixelWand

    p `setColor` "blue"

    w `readImage` "logo:"

    width <- getImageWidth w
    height <- getImageHeight w

    w `setImageBackgroundColor` p

    extentImage w 1024 768 (-(1024-width) `div` 2) (-(768-height) `div` 2)

    w `writeImage` (Just "logo_extent.png")
