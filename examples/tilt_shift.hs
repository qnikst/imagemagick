{-# LANGUAGE OverloadedStrings #-}
{-
 Implement Anthony's tilt-shift example from http://www.imagemagick.org/Usage/photos/#tilt_shift
 NOTE that I use the -function version - not the linear one
 convert beijing_md.jpg -sigmoidal-contrast 15x30% \
          \( +clone -sparse-color Barycentric '0,0 black 0,%[fx:h-1] gray80' \
             -function polynomial 4,-4,1 \) \
          -compose Blur -set option:compose:args 15 -composite \
          beijing_model.jpg
-}

import Graphics.ImageMagick.MagickWand
import qualified Data.Vector.Storable as V

main = do

    -- arguments for MagickSparseColorImage
    -- Note that the colours are stored as separate *normalized* RGB components
    let funclist = V.fromList [4,-4,1]
    withMagickWandGenesis $ do
        (_,mw) <- magickWand
        mw `readImage` "beijing_md.jpg"
        h <- getImageHeight mw
        let arglist = V.fromList [0,0, 
                        {-RGB black-} 0,0,0, 
                        0, fromIntegral (h-1), 
                        {-RGB white-} 1, 1, 1]
        -- arglist[6] = ; --TODO
        sigmoidalContrastImage mw True 15 (quantumRange*30/100)
        (_, cw) <- cloneMagickWand mw
        sparseColorImage cw (blueChannel ^|^ greenChannel ^|^ redChannel) barycentricColorInterpolate arglist
        -- Do the polynomial function
        functionImage cw polynomialFunction funclist
        -- -set option:compose:args 15
        setImageArtifact cw "compose:args" "15"

        compositeImage mw cw blurCompositeOp 0 0
        mw `writeImage` (Just "beijing_model.jpg")
