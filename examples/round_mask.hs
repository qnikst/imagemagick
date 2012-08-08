{-# LANGUAGE OverloadedStrings #-}
-- Last updated 2008/11/04 11:11

-- http://www.imagemagick.org/discourse-server/viewtopic.php?f=10&t=10993
-- convert -size 640x480 xc:none -fill white -draw 'roundRectangle 15,15 624,464 15,15' logo: -compose SrcIn -composite mask_result.png
--

import           Graphics.ImageMagick.MagickWand

main :: IO ()
main =
   withMagickWandGenesis $ do
      (_,mWand) <- magickWand
      (_,lWand) <- magickWand
      pWand     <- pixelWand
      (_,dWand) <- drawingWand
      -- Create the initial 640x480 transparent canvas
      pWand `setColor` "none"

      newImage mWand 640 480 pWand

      pWand `setColor` "white"

      dWand `setFillColor` pWand

      drawRoundRectangle dWand 15 15 624 464 15 15

      mWand `drawImage` dWand

      lWand `readImage` "logo:"

      -- Note that MagickSetImageCompose is usually only used for the MagickMontageImage
      -- function and isn't used or needed by MagickCompositeImage

      compositeImage mWand lWand srcInCompositeOp 0 0

      -- Write the new image
      writeImages mWand "mask_result.png" True
