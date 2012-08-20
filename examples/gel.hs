{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/gel.htm
-- "Gel" Effects example
-- http://www.imagemagick.org/Usage/advanced/#gel_effects

import           Prelude hiding (catch)
import           Graphics.ImageMagick.MagickWand
import           Control.Exception.Lifted
import           Control.Monad (void)


main :: IO ()
main =
  withMagickWandGenesis $ do
  -- First step is to create the gel shape:
  {-
convert -size 100x60 xc:none \
          -fill red -draw 'circle    25,30  10,30' \
                    -draw 'circle    75,30  90,30' \
                    -draw 'rectangle 25,15  75,45' \
          gel_shape.png
-}
  localGenesis $ do
    -- Create a wand
    (_,mw) <- magickWand
    pw <- pixelWand
    (_,dw) <- drawingWand

    setSize mw 100 60
    readImage mw "xc:none"

    pw `setColor` "red"
    dw `setFillColor` pw
    drawCircle dw 25 30 10 30
    drawCircle dw 75 30 90 30
    drawRectangle dw 25 15 75 45

    -- Now we draw the Drawing wand on to the Magick Wand
    drawImage mw dw

    writeImage mw (Just "gel_shape.png")

  -- Next step is to create the gel highlight:
{-
convert gel_shape.png \
            \( +clone -fx A +matte  -blur 0x12  -shade 110x0 -normalize \
               -sigmoidal-contrast 16,60% -evaluate multiply .5 \
               -roll +5+10 +clone -compose Screen -composite \) \
            -compose In  -composite  gel_highlight.png
-}
  localGenesis $ do
    (_,mw) <- magickWand
    readImage mw "gel_shape.png"

    (_,mwc) <- cloneMagickWand mw
    (_,mwf) <- fxImage mwc "A"
    -- TODO: fails, should we ignore it?
    ignoreExceptions (mw `setImageAlphaChannel` deactivateAlphaChannel) 

    blurImage mwf 0 12
    shadeImage mwf True 110 0

    normalizeImage mwf
    -- The last argument is specified as a percentage on the command line
    -- but is specified to the function as a percentage of the QuantumRange
    sigmoidalContrastImage mwf True 16 (0.6 * quantumRange)

    evaluateImage mwf multiplyEvaluateOperator 0.5
    rollImage mwf 5 10

    -- The +clone operation copies the original but only so that
    -- it can be used in the following composite operation, so we don't
    -- actually need to do a clone, just reference the original image.
    compositeImage mwf mw screenCompositeOp 0 0

    compositeImage mw mwf inCompositeOp 0 0
    writeImage mw (Just "gel_highlight.png")

  -- Now create the gel border
{-
convert gel_highlight.png \
            \( +clone -fx A  +matte -blur 0x2  -shade 0x90 -normalize \
               -blur 0x2  -negate -evaluate multiply .4 -negate -roll -.5-1 \
               +clone  -compose Multiply -composite \) \
            -compose In  -composite  gel_border.png

-}
  localGenesis $ do
    (_,mw) <- magickWand
    readImage mw "gel_highlight.png"
    (_,mwc) <- cloneMagickWand mw
    (_,mwf) <- fxImage mwc "A"
    ignoreExceptions (mwf `setImageAlphaChannel` deactivateAlphaChannel)
    blurImage mwf 0 2
    shadeImage mwf True 0 90
    normalizeImage mwf
    blurImage mwf 0 2
    negateImage mwf False
    evaluateImage mwf multiplyEvaluateOperator 0.4
    negateImage mwf False
    rollImage mwf (-0.5) (-1)
    compositeImage mwf mw multiplyCompositeOp 0 0
    compositeImage mw mwf inCompositeOp 0 0
    writeImage mw (Just "gel_border.png")

  -- and finally the text and shadow effect
{-
  convert gel_border.png \
      -font Candice  -pointsize 24  -fill white  -stroke black \
      -gravity Center  -annotate 0 "Gel"  -trim -repage 0x0+4+4 \
      \( +clone -background navy -shadow 80x4+4+4 \) +swap \
      -background none  -flatten    gel_button.png
-}
  localGenesis $ do
    (_,mw) <- magickWand
    (_,dw) <- drawingWand
    pw <- pixelWand
    readImage mw "gel_border.png"
    dw `setFont` "Lucida-Handwriting-Italic"
    dw `setFontSize` 24
    pw `setColor` "white"
    dw `setFillColor` pw
    pw `setColor` "black"
    dw `setStrokeColor` pw
    dw `setGravity` centerGravity
    -- It is important to notice here that MagickAnnotateImage renders the text on
    -- to the MagickWand, NOT the DrawingWand. It only uses the DrawingWand for font
    -- and colour information etc.
    annotateImage mw dw 0 0 0 "Gel"
    trimImage mw 0
    resetImagePage mw "0x0+4+4"
    (_,mwc) <- cloneMagickWand mw
    pw `setColor` "navy"
    mwc `setImageBackgroundColor` pw
    shadowImage mwc 80 4 4 4
    (_,mwf) <- magickWand
    addImage mwf mwc
    addImage mwf mw
    pw `setColor` "none"
    mwf `setImageBackgroundColor` pw
    (_,mw') <- mergeImageLayers mwf flattenLayer
    writeImage mw' (Just "gel_button.png")

ignoreExceptions f = catch (void f) (\(_::ImageWandException) -> return ())
