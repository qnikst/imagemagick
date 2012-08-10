{-# LANGUAGE OverloadedStrings #-}
-- http://members.shaw.ca/el.supremo/MagickWand/3dlogo.htm

-- Better 3-D Logo Generation example
-- http://www.imagemagick.org/Usage/advanced/#3d-logos-2

import Graphics.ImageMagick.MagickWand
import           Graphics.ImageMagick.MagickWand.FFI.Types    -- remove



main = do
    withMagickWandGenesis $ do
      localGenesis $ do
        {-
        convert -size 170x100 xc:black \
                  -fill white -draw 'circle    50,50  13,50' \
                              -draw 'circle   120,50 157,50' \
                              -draw 'rectangle 50,13 120,87' \
                  -fill black -draw 'circle    50,50  25,50' \
                              -draw 'circle   120,50 145,50' \
                              -draw 'rectangle 50,25 120,75' \
                  -fill white -draw 'circle    60,50  40,50' \
                              -draw 'circle   110,50 130,50' \
                              -draw 'rectangle 60,30 110,70' \
                  -gaussian 1x1 +matte logo_mask.png
        -}

        (_,mw) <- magickWand
        pw <- pixelWand
        (_,dw) <- drawingWand

        setSize mw 170 100
        mw `readImage` "xc:black"

        pw `setColor` "white"
        dw `setFillColor` pw

        drawCircle dw 50 50 13 50
        drawCircle dw 120 50 157 50
        drawRectangle dw 50 13 120 87

        pw `setColor` "black"
        
        dw `setFillColor` pw
        drawCircle dw 50 50 25 50
        drawCircle dw 50 50 25 50
        drawCircle dw 120 50 145 50
        drawRectangle dw 50 25 120 75

        pw `setColor` "white" 
        dw `setFillColor` pw
        drawCircle dw 60 50 40 50
        drawCircle dw 110 50 130 50
        drawRectangle dw 60 30 110 70

        -- Now we draw the Drawing wand on to the Magick Wand
        mw `drawImage` dw

        gaussianBlurImage mw 1 1 
        -- Turn the matte of == +matte
        mw `setImageMatte` False

        mw `writeImage` (Just "logo_mask.png")

    localGenesis $ do
    
        (_,mw)  <- magickWand
        (_,mwc) <- magickWand
        pw  <- pixelWand
        (_,dw)  <- drawingWand
        {-
        convert ant_mask.png -fill red -draw 'color 0,0 reset' \
                  ant_mask.png +matte  -compose CopyOpacity -composite \
                  -font Candice  -pointsize 36  -fill white  -stroke black \
                  -gravity Center  -annotate 0 "Ant" \
                  ant.png
        -}

        mw `readImage` "logo_mask.png"

        pw `setColor` "red"
        dw `setFillColor` pw

        drawColor dw  0 0 resetMethod
        mw `drawImage` dw

        mwc `readImage` "logo_mask.png"
        mwc `setImageMatte` False

        compositeImage mw mwc copyOpacityCompositeOp 0 0

        -- Annotate gets all the font information from the drawingwand
        -- but draws the text on the magickwand
        -- I haven't got the Candice font so I'll use a pretty one
        -- that I know I have
        dw `setFont` "Lucida-Handwriting-Italic"
        dw `setFontSize` 36
        pw `setColor` "white"
        dw `setFillColor` pw
    
        pw `setColor` "black"
        dw `setStrokeColor` pw
        dw `setGravity` centerGravity
        annotateImage mw dw 0 0 0 "Ant"
        mw `writeImage` (Just "logo_ant.png")

{-
convert ant.png  -fx A  +matte -blur 0x6  -shade 110x30  -normalize \
          ant.png  -compose Overlay -composite \
          ant.png  -matte  -compose Dst_In  -composite \
          ant_3D.png
-}
    localGenesis $ do
        (_,mw) <- magickWand
        mw `readImage` "logo_ant.png"
        (_,mwf) <- fxImage mw "A"

        --  MagickSetImageMatte(mw,MagickFalse);
        -- +matte is the same as -alpha off
        -- mwf `setImageAlphaChannel` deactivateAlphaChannel
        blurImage mwf 0 6
        shadeImage mwf True 110 30
        normalizeImage mwf
        -- ant.png  -compose Overlay -composite 
        (destroyMwc, mwc) <- magickWand
        mwc `readImage` "logo_ant.png"
        compositeImage mwf mwc overlayCompositeOp 0 0
        
        -- ant.png  -matte  -compose Dst_In  -composite
        mwc' <- magickWand
        mwc `readImage` "logo_ant.png"
        -- -matte is the same as -alpha on
        -- I don't understand why the -matte in the command line
        -- does NOT operate on the image just read in (logo_ant.png in mwc)
        -- but on the image before it in the list
        -- It would appear that the -matte affects each wand currently in the
        -- command list because applying it to both wands gives the same result

        -- setImageAlphaChannel mwf setAlphaChannel
        -- setImageAlphaChannel mwc setAlphaChannel
        compositeImage mwf mwc dstInCompositeOp 0 0 

        writeImage mwf (Just "logo_ant_3D.png")


    {- Now for the shadow
    convert ant_3D.png \( +clone -background navy -shadow 80x4+6+6 \) +swap \
              -background none  -layers merge +repage ant_3D_shadowed.png
    -}
    localGenesis $ do
        pw <- pixelWand
        (_,mw) <- magickWand
        readImage mw "logo_ant_3D.png"

        (_,mwc) <- cloneMagickWand mw

        pw `setColor` "navy"
        mwc `setImageBackgroundColor` pw

        shadowImage mwc 80 4 6 6 
        
        -- at this point
        -- mw = ant_3D.png
        -- mwc = +clone -background navy -shadow 80x4+6+6
        -- To do the +swap I create a new blank MagickWand and then
        -- put mwc and mw into it. ImageMagick probably doesn't do it
        -- this way but it works here and that's good enough for me!
        (_,mwf) <- magickWand
        mwf `addImage` mwc
        mwf `addImage` mw

        pw `setColor` "none"
        setImageBackgroundColor mwf pw
        (_,mwc') <- mergeImageLayers mwf mergeLayer
        mwc' `writeImage` (Just "logo_shadow_3D.png")


    {-
    and now for the fancy background
    convert ant_3D_shadowed.png \
              \( +clone +repage +matte   -fx 'rand()' -shade 120x30 \
                 -fill grey70 -colorize 60 \
                 -fill lavender -tint 100 \) -insert 0 \
              -flatten  ant_3D_bg.jpg
    -}
    localGenesis $ do
        pw <- pixelWand
        (_,mw) <- magickWand
        mw `readImage` "logo_shadow_3D.png"

        (_,mwc) <- cloneMagickWand mw
        -- +repage
        resetImagePage mwc "" 
        -- +matte is the same as -alpha off
        -- setImageAlphaChannel mwc deactivateAlphaChannel
        (_, mwf) <- fxImage mwc "rand()"

        shadeImage mwf True 120 30
        setColor pw "grey70"
        --  It seems that this must be a separate pixelwand for Colorize to work!
        pwo <- pixelWand
        -- AHA .. this is how to do a 60% colorize
        pwo `setColor` "rgb(60%,60%,60%)"
        colorizeImage mwf pw pwo

        pw `setColor` "lavender"
        -- and this is a 100% tint
        pwo `setColor` "rgb(100%,100%,100%)"
        tintImage mwf pw pwo 

        (_, mwc') <- magickWand
        mwc' `addImage` mwf
        mwc' `addImage` mwc 

        (_, mwf') <- mergeImageLayers mwc flattenLayer
        mwf' `writeImage` (Just "logo_bg_3D.jpg")

