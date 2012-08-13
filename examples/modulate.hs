{-# LANGUAGE OverloadedStrings #-}
{-
 From: http://www.imagemagick.org/discourse-server/viewtopic.php?f=1&t=12640

    Read the logo: image and use PixelIterators to produce two new images.
    Convert the image to the HSB and HSL colourspaces, cut the Lightness/Brightness
    by half and then write the images as logo_hsb.jpg and logo_hsl.jpg
    Note the colour distortion in the HSL result whereas the HSB image is what
    we'd expect to see when reducing an image's brightness by 50%.
    
    As with my other examples, there is no error checking in this code.
    If you adapt this to your own use, you should add error checking to ensure
    that, for example, MagickReadImage succeeds and that the width and height
    of the image in mw are reasonable values.
-}
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Vector.Storable as V
import Graphics.ImageMagick.MagickWand

main  = withMagickWandGenesis $ do
    (_,mw) <- magickWand
    
    mw `readImage` "logo:"
    
    width <- getImageWidth mw
    height <- getImageHeight mw

    (_,mwl) <- magickWand
    (_,mwb) <- magickWand

    p <- pixelWand
    p `setColor` "none"
    -- Set the hsl and hsb images to the same size as the input image
    newImage mwl width height p
    newImage mwb width height p
    -- Even though we won't be reading these images they must be initialized
    -- to something TODO: fails to work
    -- readImage mwl "xs:none"
    -- readImage mwb  "xs:none"

    -- Create iterators for each image
    (_,imw)  <- pixelIterator mw 
    (_,imwl) <- pixelIterator mwl
    (_,imwb) <- pixelIterator mwb
    
    it1 <- pixelIterateList imw
    it2 <- pixelIterateList imwl
    it3 <- pixelIterateList imwb
    mapM_ (action imwl imwb) $ zip3 it1 it2 it3
    
    -- write the results
    mwb `writeImage` (Just "logo_hsb.jpg")
    mwl `writeImage` (Just "logo_hsl.jpg")
    where
      action imwl imwb (pmw, pmwl, pmwb) = do
        V.zipWithM_ inner1 pmw pmwb 
        V.zipWithM_ inner2 pmw pmwl
        -- Sync writes the pixels back to the magick wands
        pixelSyncIterator imwl
        pixelSyncIterator imwb
      inner1 xmw xmwb = localGenesis $ do
            -- Get the RGB quanta from the source image
            qr <- getRedQuantum xmw
            qg <- getGreenQuantum xmw
            qb <- getBlueQuantum xmw

            -- Convert the source quanta to HSB
            (bh,bs,bb) <- convertRGBToHSB qr qg qb
            (qr1,qg1,qb1) <- convertHSBToRGB bh bs (0.5*bb)
            -- Set the pixel in the HSB output image
            xmwb `setRedQuantum` qr1
            xmwb `setGreenQuantum` qg1
            xmwb `setBlueQuantum` qb1
      inner2 xmw xmwl = localGenesis $ do
            qr <- getRedQuantum xmw
            qg <- getGreenQuantum xmw
            qb <- getBlueQuantum xmw
            -- Convert the source quanta to HSL
            (lh,ls,ll) <- convertRGBToHSL qr qg qb
            (qr2,qg2,qb2) <- convertHSLToRGB lh ls (ll*0.5)
            -- Set the pixel in the HSL output image
            xmwl `setRedQuantum` qr2
            xmwl `setGreenQuantum` qg2
            xmwl `setBlueQuantum` qb2

zipWith3M_ f a b c = V.zipWithM_ f (V.zipWith g a b) c
  where g a b = (a,b)
        f' f (a,b) c = f a b c
