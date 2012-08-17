{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/resize.htm
--
-- convert logo: -filter lanczos -resize 50% -quality 95 logo_resize.jpg
-- Read an image, resize it by 50% and sharpen it, and then save as
-- a high quality JPG
-- Note that ImageMagick's default quality is 75.

import           Graphics.ImageMagick.MagickWand

main :: IO ()
main = do
  withMagickWandGenesis $ do
    (_,w) <- magickWand

    -- Read the image
    readImage w "logo:"

    -- Cut them in half but make sure they don't underflow
    width  <- fmap safeHalf (getImageWidth w)
    height <- fmap safeHalf (getImageHeight w)

    -- Resize the image using the Lanczos filter
    -- The blur factor is a 'Double', where > 1 is blurry, < 1 is sharp
    -- I haven't figured out how you would change the blur parameter of MagickResizeImage
    -- on the command line so I have set it to its default of one.
    resizeImage w width height lanczosFilter 1

    -- Set the compression quality to 95 (high quality = low compression)
    setImageCompressionQuality w 95

    -- Write the new image
    writeImages w "logo_resize.jpg" True

  where
    safeHalf = max 1 . (`div`2)

