import System.Environment
import Graphics.ImageMagick.MagickWand.Internal -- TODO: types
import Graphics.ImageMagick.MagickWand
import Control.Monad
import Filesystem.Path.CurrentOS


main = do
  [img,out] <- getArgs
  withMagickWandGenesis $
    withMagickWand $ do
      stR <- readImage $ decodeString img
--      unless stR $ throwWandException 
      magickIterate $ do
        resizeImage 106 80 lanczosFilter 1.0
      writeImages (decodeString out) True
--      unless stW $ throwWandException

