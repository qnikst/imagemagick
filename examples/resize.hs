-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/resize.htm
--
-- convert logo: -filter lanczos -resize 50% -quality 95 logo_resize.jpg
-- Read an image, resize it by 50% and sharpen it, and then save as
-- a high quality JPG
-- Note that ImageMagick's default quality is 75.

import Graphics.ImageMagick.MagickWand
import System.Environment (getArgs)
import Filesystem.Path.CurrentOS (decodeString)

main = do
  [img,img'] <- getArgs
  withMagickWandGenesis $ do
    (_,w) <- magickWand
    readImage w (decodeString img')

    -- Cut them in half but make sure they don't underflow
    width  <- fmap safeHalf (getImageWidth w)
    height <- fmap safeHalf (getImageHeight w)

    resizeImage w width height lanczosFilter 1

    setImageCompressionQuality w 95

    writeImages w (decodeString img') True

  where 
    safeHalf = max 1 . (`div`2)
    
