{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/draw_shapes.htm
-- There's no equivalent convert command for this. It is a demo of MagickWand.
-- Bits of this were pinched from
-- http://www.imagemagick.org/api/MagickWand/drawtest_8c-source.html

import           Graphics.ImageMagick.MagickWand
import           Graphics.ImageMagick.MagickWand.FFI.Types


main :: IO ()
main =
  withMagickWandGenesis $ do
    (_,w) <- magickWand
    (_,dw) <- drawingWand
    c <- pixelWand

    let diameter = 640
        radius = (fromIntegral diameter) / 2

    c `setColor` "white"
    newImage w diameter diameter c

    dw `setStrokeOpacity` 1
    -- circle and rectangle
    pushDrawingWand dw

    -- Hmmmm. Very weird. rgb(0,0,1) draws a black line around the edge
    -- of the circle as it should. But rgb(0,0,0) or black don't.
    -- AND if I remove the PixelSetColor then it draws a white boundary
    -- around the rectangle (and presumably around the circle too)
    c `setColor` "rgb(0,0,1)"

    dw `setStrokeColor` c
    dw `setStrokeWidth` 4
    dw `setStrokeAntialias` True
    c `setColor` "red"
    -- dw `setStrokeOpacity` 1
    dw `setFillColor` c

    drawCircle dw radius radius radius (radius * 2)
    drawRectangle dw 50 13 120 87
    popDrawingWand dw

    -- rounded rectangle
    pushDrawingWand dw
    let poly1 = [
          PointInfo 378.1 81.72, PointInfo 381.1 79.56, PointInfo 384.3 78.12, PointInfo 387.6 77.33,
          PointInfo 391.1 77.11, PointInfo 394.6 77.62, PointInfo 397.8 78.77, PointInfo 400.9 80.57,
          PointInfo 403.6 83.02, PointInfo 523.9 216.8, PointInfo 526.2 219.7, PointInfo 527.6 223,
          PointInfo 528.4 226.4, PointInfo 528.6 229.8, PointInfo 528   233.3, PointInfo 526.9 236.5,
          PointInfo 525.1 239.5, PointInfo 522.6 242.2, PointInfo 495.9 266.3, PointInfo 493   268.5,
          PointInfo 489.7 269.9, PointInfo 486.4 270.8, PointInfo 482.9 270.9, PointInfo 479.5 270.4,
          PointInfo 476.2 269.3, PointInfo 473.2 267.5, PointInfo 470.4 265,   PointInfo 350   131.2,
          PointInfo 347.8 128.3, PointInfo 346.4 125.1, PointInfo 345.6 121.7, PointInfo 345.4 118.2,
          PointInfo 346   114.8, PointInfo 347.1 111.5, PointInfo 348.9 108.5, PointInfo 351.4 105.8,
          PointInfo 378.1 81.72
          ]

    dw `setStrokeAntialias` True
    dw `setStrokeWidth` 2.016
    dw `setStrokeLineCap` roundCap
    dw `setStrokeLineJoin` roundJoin
    dw `setStrokeDashArray` []
    c `setColor`{- "#000080" -} "rgb(0,0,128)"
    -- If strokecolor is removed completely then the circle is not there
    dw `setStrokeColor` c
    -- But now I've added strokeopacity - 1=circle there  0=circle not there
    -- If opacity is 1 the black edge around the rectangle is visible
    dw `setStrokeOpacity` 1
    -- No effect
    {- dw `setFillRule` evenOddRule -}
    -- this doesn't affect the circle
    c `setColor` "#c2c280" {- "rgb(194,194,128)" -}
    dw `setFillColor` c
    -- 1=circle there 0=circle there but rectangle fill disappears
    -- dw `setFillOpacity` False
    drawPolygon dw poly1
    -- dw `setFillOpacity` True
    popDrawingWand dw

    pushDrawingWand dw
    -- yellow polygon
    let poly2 = [
          PointInfo 540   288,   PointInfo 561.6 216,   PointInfo 547.2 43.2,  PointInfo 280.8 36,
          PointInfo 302.4 194.4, PointInfo 331.2 64.8,  PointInfo 504   64.8,  PointInfo 475.2 115.2,
          PointInfo 525.6 93.6,  PointInfo 496.8 158.4, PointInfo 532.8 136.8, PointInfo 518.4 180,
          PointInfo 540   172.8, PointInfo 540   223.2, PointInfo 540   288
          ]
    dw `setStrokeAntialias` True
    dw `setStrokeWidth` 5.976
    dw `setStrokeLineCap` roundCap
    dw `setStrokeLineJoin` roundJoin
    dw `setStrokeDashArray` []
    c `setColor` "#4000c2"
    dw `setStrokeColor` c
    dw `setFillRule` evenOddRule
    c `setColor` "#ffff00"
    dw `setFillColor` c
    drawPolygon dw poly2
    popDrawingWand dw

    -- rotated and translated ellipse
    -- The DrawEllipse function only draws the ellipse with
    -- the major and minor axes orthogonally aligned. This also
    -- applies to some of the other functions such as DrawRectangle.
    -- If you want an ellipse that has the major axis rotated, you
    -- have to rotate the coordinate system before the ellipse is
    -- drawn. And you'll also want the ellipse somewhere on the
    -- image rather than at the top left (where the 0,0 origin is
    -- located) so before drawing the ellipse we move the origin to
    -- wherever we want the centre of the ellipse to be and then
    -- rotate the coordinate system by the angle of rotation we wish
    -- to apply to the ellipse and *then* we draw the ellipse.
    -- NOTE that doing all this within `pushDrawingWand`/`popDrawingWand`
    -- means that the coordinate system will be restored after
    -- the `popDrawingWand`
    pushDrawingWand dw
    c `setColor` "rgb(0,0,1)"
    dw `setStrokeColor` c
    dw `setStrokeWidth` 2
    dw `setStrokeAntialias` True
    c `setColor` "orange"
    -- dw `setStrokeOpacity` 1
    dw `setFillColor` c
    -- Be careful of the order in which you meddle with the
    -- coordinate system! Rotating and then translating is
    -- not the same as translating then rotating
    translate dw (radius/2) (3 * radius/2)
    rotate dw (-30)
    drawEllipse dw 0 0 (radius/8) (3*radius/8) 0 360
    popDrawingWand dw

    -- A line from the centre of the circle
    -- to the top left edge of the image
    drawLine dw 0 0 radius radius

    drawImage w dw

    writeImage w "chart_test.jpg"
