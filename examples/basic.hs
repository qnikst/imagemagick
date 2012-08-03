import           Control.Monad
import           Filesystem.Path.CurrentOS
import           Graphics.ImageMagick.MagickWand
import           Graphics.ImageMagick.MagickWand.Internal
import           System.Environment


main = do
  [img,out] <- getArgs
  withMagickWandGenesis $ do
    (_,w) <- magickWand
    stR <- readImage w $ decodeString img
--      unless stR $ throwWandException
    magickIterate w $ \p -> do
        resizeImage p 106 80 lanczosFilter 1.0
        return ()
    writeImages w (decodeString out) True
--      unless stW $ throwWandException
    return ()

