{-# LANGUAGE OverloadedStrings #-}
--  http://members.shaw.ca/el.supremo/MagickWand/trans_paint.htm
--  Last updated 2008/11/25 08:48
-- 
--  Use MagickTransparentPaintImage to change *all* white pixels
-- to transparent in the logo: image

import Graphics.ImageMagick.MagickWand --magick wand bindings

main = do
    -- start image magick block
    withMagickWandGenesis $ do
        (_, w) <- magickWand
        readImage w "logo: "
        -- Set up the pixelwand containing the colour to be "targeted"
        -- by transparency
        t <- pixelWand
        t `setColor` "white"

        -- Change the transparency of all colours which match target (with
        -- fuzz applied). In this case they are made completely transparent (0)
        --  but you can set this to any value from 0 to 1.
        transparentPaintImage w t 0 fuzz False

        writeImages w "logo_white.png" True
    where
        -- A larger fuzz value allows more colours "near" white to be
        -- modified. A fuzz of zero only allows an exact match with the 
        -- given colour
        fuzz = 0
