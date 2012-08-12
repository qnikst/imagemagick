{-# LANGUAGE OverloadedStrings #-}
-- 
--    Create a simple grayscale gradient using Pixel Iterators
--
import qualified Data.Vector.Storable as V
import Control.Monad
import Text.Printf
import Data.ByteString.Char8 as S
import Graphics.ImageMagick.MagickWand

main = withMagickWandGenesis $ do
    pWand <- pixelWand
    pWand `setColor` "white"
    (_,mWand) <- magickWand
    -- Create a 100x100 image with a default of white
    newImage mWand 100 100 pWand
    -- Get a new pixel iterator 
    
    (_,it)  <- pixelIterator mWand
    forM_ [1..100] $ \_ -> do
        (_,pixels)  <- pixelGetNextIteratorRow it
        (flip imapM_) pixels $ \x v -> do
            let gray = x*255 `div` 100
                hex  = S.pack $ '#':(printf "%02x%02x%02x" gray gray gray)
            v `setColor` hex
        -- Sync writes the pixels back to the m_wand
        pixelSyncIterator it
    mWand `writeImage` (Just "bits_demo.gif")

imapM_ :: (Monad m, V.Storable a) => (Int -> a -> m ()) -> V.Vector a -> m ()
imapM_ f v = V.foldM'_ (\x a -> f x a >> return (x+1)) 1 v  
