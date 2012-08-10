{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/reflect.htm
{- convert logo: '(' logo: -resize 100%x50%! -flip -size  640x240 \
		gradient:white-black +matte -compose copyopacity -composite ')'  \
		-append logo_reflect.png -}
import           Graphics.ImageMagick.MagickWand
import           Graphics.ImageMagick.MagickWand.FFI.Types

main :: IO ()
main = withMagickWandGenesis $ do
  (_,mw) <- magickWand
  readImage mw "logo:"
  -- We know that logo: is 640x480 but in the general case
  -- we need to get the dimensions of the image
  w <- getImageWidth mw
  h <- getImageHeight mw

  -- +matte is the same as -alpha off
  -- This does it the "new" way but if your IM doesn't have this
  -- then MagickSetImageMatte(mw,MagickFalse); can be used
  -- TODO: fails, should we ignore it?
  mw `setImageAlphaChannel` deactivateAlphaChannel
  -- clone the input image
  (_,mwr) <- magickWand--cloneMagickWand mw
  readImage mwr "logo:"
  -- Resize it
  resizeImage mwr w (h `div` 2) lanczosFilter 1
  -- Flip the image over to form the reflection
  flipImage mwr
  -- Create the gradient image which will be used as the alpha
  -- channel in the reflection image
  (_,mwg) <- magickWand
  setSize mwg w (h `div` 2)
  readImage mwg "gradient:white-black"

  -- Copy the gradient in to the alpha channel of the reflection image
  compositeImage mwr mwg copyOpacityCompositeOp 0 0

  -- Add the reflection image to the wand which holds the original image
  addImage mw mwr

  -- Append the reflection to the bottom (MagickTrue) of the original image
  (_,mwg') <- appendImages mw True

  -- and save the result
  writeImage mwg' (Just "logo_reflect.png")
