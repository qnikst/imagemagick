{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/bunny.htm
-- This implements the command:
-- convert bunny_grass.gif ( bunny_anim.gif -repage 0x0+5+15! ) \
--            -coalesce -delete 0 -deconstruct -loop 0  bunny_bgnd.gif
-- from Anthony's examples at: http://www.imagemagick.org/Usage/anim_basics/#cleared

import           Control.Monad                   (forM_)
import           Control.Monad.Trans.Resource    (release)
import           Graphics.ImageMagick.MagickWand

main :: IO ()
main = withMagickWandGenesis $ do
  {- Create a wand -}
  (mw0k,mw0) <- magickWand

  {- Read the first input image -}
  readImage mw0 "bunny_grass.gif"

  --( bunny_anim.gif -repage 0x0+5+15\! )
  -- We need a separate wand to do this bit in parentheses
  localGenesis $ do
    (_,aw) <- magickWand
    readImage aw "bunny_anim.gif"
    resetImagePage aw "0x0+5+15!"

    -- Now we have to add the images in the aw wand on to the end
    -- of the mw wand.
    addImage mw0 aw
    -- thee aw wand is destoyed on exiting `localGenesis` so that it can be used
    -- for the next operation


  -- -coalesce
  (aw0k, aw0) <- coalesceImages mw0

  -- do "-delete 0" by copying the images from the "aw" wand to
  -- the "mw" wand but omit the first one
  -- free up the mw wand and recreate it for this step
  release mw0k
  (_,mw1) <- magickWand
  n <- getNumberImages aw0
  forM_ [1..(n-1)] $ \i -> localGenesis $ do
    aw0 `setIteratorIndex` i
    (_,tw) <- getImage aw0
    addImage mw1 tw

  resetIterator mw1

  -- free up aw for the next step
  release aw0k

  -- -deconstruct
  -- Anthony says that MagickDeconstructImages is equivalent
  -- to MagickCompareImagesLayers so we'll use that

  (_,aw1) <- compareImageLayers mw1 compareAnyLayer

  -- -loop 0
  setOption aw1 "loop" "0"

  {- write the images into one file -}
  writeImages aw1 "bunny_bgnd.gif" True
