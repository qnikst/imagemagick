{-# LANGUAGE OverloadedStrings #-}
import           Filesystem.Path.CurrentOS
import           Graphics.ImageMagick.MagickWand
import           System.Environment

main = do
  [img,img'] <- getArgs

  withMagickWandGenesis $ do
    (_,w) <- magickWand
    p <- pixelWand

    p `setColor` "blue"

    w `readImage` decodeString img

    width <- getImageWidth w
    height <- getImageHeight w

    w `setImageBackgroundColor` p

    extentImage w 1024 768 (-(1024-width) `div` 2) (-(768-height) `div` 2)

    writeImages w (decodeString img') True
