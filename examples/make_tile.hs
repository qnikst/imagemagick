{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
-- http://members.shaw.ca/el.supremo/MagickWand/make_tile.htm
-- The program makes two tiles, one using the plasma: pseudo file and one using noise
-- See: http://www.imagemagick.org/Usage/canvas/#plasma_seeded
-- and: http://www.imagemagick.org/Usage/canvas/#random

--  convert -size 100x100  plasma:red-yellow ( +clone -flop ) +append \
--      ( +clone -flip ) -append -resize 50% tile_plasma.png
-- and
-- convert -size 100x100 xc: +noise Random -virtual-pixel tile \
--      -blur 0x10 -normalize ( +clone -flop ) +append \
--      ( +clone -flip ) -append -resize 50% tile_random.png

{-
 Basically this flops and flips the image and joins the four results together
 and then resizes the result by 50% so that it's the same size as the original.
 E.g. if the original image is "/", it creates the flop image "\" and then appends
 them side by side to give "/\". Then it takes this image and flips it which produces
 "\/" and then appends these one on top of the other to produce

  /\
  \/

 and finally, since this image is now twice the size of the original, it is resized to 50%.
-}

import Graphics.ImageMagick.MagickWand
import Control.Monad.Trans.Resource
import Control.Monad (void)

-- make-tile creates a tileable image from an input image.
-- ( +clone -flop ) +append  ( +clone -flip ) -append -resize 50%
makeTile mw outfile = localGenesis $ do
    (destroyMwc, mwc) <- cloneMagickWand mw
    flopImage mwc
    mw `addImage` mwc
    release destroyMwc

    mwc' <- mw `appendImages` False
    (destroyMwf, mwf) <- cloneMagickWand mwc
    flipImage mwf
    mwc `addImage` mwf
    release destroyMwf

    (_,mwf') <- mwc `appendImages` True

    w <- getImageWidth mwf'
    h <- getImageHeight mwf'
    -- 1 = Don't blur or sharpen image
    resizeImage mwf (w `div` 2) (h `div` 2) lanczosFilter 1
    mwf `writeImage` outfile

main = do
    withMagickWandGenesis $ do
      (destroyMw, mw) <- magickWand
      setSize mw 100 100
      readImage mw "plasma:red-yellow"
      makeTile mw (Just "tile_plasma.png")
      release destroyMw

      (destroyMw', mw') <- magickWand
      setSize mw' 100 100
      mw `readImage` "xc:"
      mw `addNoiseImage` randomNoise
      void $ mw `setVirtualPixelMethod` tileVirtualPixelMethod
      blurImage mw 0 10
      normalizeImage mw
      makeTile mw (Just "tile_random.png")

