{-# LANGUAGE OverloadedStrings #-}
-- | http://www.imagemagick.org/discourse-server/viewtopic.php?f=10&t=12285
-- and more recently: http://www.imagemagick.org/discourse-server/viewtopic.php?f=1&t=16783
-- http://www.imagemagick.org/Usage/channels/#masked_compose
-- Replicate a masked composite:
--  convert -size 100x100 tile:tile_water.jpg tile:tile_disks.jpg \
--    mask_bite.png -composite compose_masked.png
--

import           Graphics.ImageMagick.MagickWand
import           Graphics.ImageMagick.MagickCore

main = do
  -- MagickWand *dest = NULL, *src = NULL, *mask = NULL;

  withMagickWandGenesis $ do

    -- Create the wands
    (_,dest) <- magickWand
    (_,mask) <- magickWand
    (_,src)  <- magickWand

    setSize dest 100 100
    setSize src 100 100

    readImage dest "tile:tile_water.jpg" -- tile: ?
    readImage mask "mask_bite.png"       -- ?

    -- When you create a mask, you use white for those parts that you want
    -- to show through and black for those which must not show through.
    -- But internally it's the opposite so the mask must be negated

    negateImage mask False
    setImageClipMask dest mask

    readImage src "tile:tile_disks.jpg"

    -- This does the src (overlay) over the dest (background)
    compositeImage dest src overCompositeOp 0 0

    writeImages dest "clip_out.jpg" True
