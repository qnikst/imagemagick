{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/affine.htm
{-
	Originally inspired by:
http://www.imagemagick.org/discourse-server/viewtopic.php?f=2&t=12530
	The idea for these specific examples came from reading this:
http://www.csl.mtu.edu/cs4611/www/HillLectureNotes/CS4611%202D%20Affine%20Transformation.htm
	When reading that (and other web pages about affine) keep in mind
	that IM's ordering of the affine matrix as described at:
	http://imagemagick.org/script/command-line-options.php#affine
	orders the affine values and their multiplication like this:
	[x y 1] |sx rx 0|
			|ry sy 0|
			|tx ty 1|

	Whereas the CS4611 web page uses this (which, if nothing else, is tidier):
	|sx ry tx| |x|
	|rx sy ty| |y|
	|0  0  1 | |1|

	My multiplication routine is written to conform to the way IM
	specifies things.

	ALSO, I think there are a couple of errors on the CS4611 page.
	1. In the example of rotation about a point, it says that first
		translate by V, then rotate, then translate by -V.
		But the matrix representation of this does -V,rotate,V.
	2. Reflection across the x-axis is not correct as shown.
		When a point (x,y) is reflected across the x-axis its
		new coordinate is (x,-y) - the matrix shown in the example
		actually reflects across the y-axis - i.e. it produces (-x,y).
-}

import           Data.Fixed                      (mod')
import           Graphics.ImageMagick.MagickWand

-- typesafe angle logic could be imported form AC-Angle package

-- | Convert from degrees to radians.
radians :: Double -> Double
radians x = x / 180 * pi

-- | Convert from radians to degrees.
degrees :: Double -> Double
degrees x = x * 180 / pi

-- Set the affine array to translate by (x,y)
-- Set the affine array to scale the image by sx,sy
translate_affine :: (Floating x) => x -> x -> [x]
translate_affine x y =
  [ 1, 0, 0,
    1, x, y ]

-- Set the affine array to scale the image by sx,sy
scale_affine :: (Floating x) => x -> x -> [x]
scale_affine sx sy =
  [ sx, 0, 0,
    sy, 0, 0 ]

-- get the affine array to rotate image by 'degrees' clockwise
rotate_affine :: Double -> [Double]
rotate_affine angle =
  [ cos (radians (angle `mod'` 360)),  sin (radians (angle `mod'` 360)),  -sin (radians (angle `mod'` 360)),
    cos (radians (angle `mod'` 360)),                             0,                              0 ]

-- Multiply two affine arrays and return the result.
affine_multiply :: (Floating x) => [x] -> [x] -> [x]
affine_multiply [a0,a1,a2,a3,a4,a5] [b0,b1,b2,b3,b4,b5] =
  [      a0*b0 + a1*b2,      a0*b1 + a1*b3,
         a2*b0 + a3*b2,      a2*b1 + a3*b3,
    a4*b0 + a5*b2 + b4, a4*b1 + a5*b3 + b5 ]
affine_multiply _ _ = error "incorrect list sizes"

main :: IO ()
main = withMagickWandGenesis $ do
  -- Remember that these operations are done with respect to the
  -- origin of the image which is the TOP LEFT CORNER.
  localGenesis $ do
    -- Example 1.
    -- Rotate logo: by 90 degrees (about the origin), scale by 50 percent and
    -- then move the image 240 in the x direction
    -- TODO: fix problem with 'leaky' pixel
    (_,mw) <- magickWand
    readImage mw "logo:"
    -- Set up the affine matrices
    -- rotate 90 degrees clockwise
    let
      r = rotate_affine 90
    -- scale by .5 in x and y
      s = scale_affine 0.5 0.5
    -- translate to (240,0)
      t = translate_affine 240 0
    -- now multiply them - note the order in
    -- which they are specified - in particular beware that
    -- temp = r*s is NOT necessarily the same as temp = s*r

    --first do the rotation and scaling
    -- temp = r*s
      temp = r `affine_multiply` s
    -- now the translation
    -- result = temp*t;
      result = temp `affine_multiply` t

    -- and then apply the result to the image
    distortImage mw affineProjectionDistortion result False

    writeImage mw (Just "logo_affine_1.jpg")

  localGenesis $ do
    -- Example 2
    -- Rotate logo: 30 degrees around the point (300,100)
    -- Since rotation is done around the origin, we must translate
    -- the point (300,100) up to the origin, do the rotation, and
    -- then translate back again
    (_,mw) <- magickWand
    readImage mw "logo:"

    let
      -- Initialize the required affines
      -- translate (300,100) to origin
      t1 = translate_affine (-300) (-100)
      -- rotate clockwise by 30 degrees
      r = rotate_affine 30
      -- translate back again
      t2 = translate_affine 300 100
      -- Now multiply the affine sequence
      -- temp = t1*r
      temp = t1 `affine_multiply` r
      -- result = temp*t2;
      result = temp `affine_multiply` t2

    distortImage mw affineProjectionDistortion result False

    writeImage mw (Just "logo_affine_2.jpg")

  localGenesis $ do
    -- Example 3
    -- Reflect the image about a line passing through the origin.
    -- If the line makes an angle of D degrees with the horizontal
    -- then this can be done by rotating the image by -D degrees so
    -- that the line is now (in effect) the x axis, reflect the image
    -- across the x axis, and then rotate everything back again.
    -- In this example, rather than just picking an arbitrary angle,
    -- let's say that we want the "logo:" image to be reflected across
    -- it's own major diagonal. Although we know the logo: image is
    -- 640x480 let's also generalize the code slightly so that it
    -- will still work if the name of the input image is changed.
    -- If the image has a width "w" and height "h", then the angle between
    -- the x-axis and the major diagonal is atan(h/w) (NOTE that this
    -- result is in RADIANS!)
    -- For this example I will also retain the original dimensions of the
    -- image so that anything that is reflected outside the borders of the
    -- input image is lost
    (_,mw) <- magickWand
    readImage mw "logo:"
    w <- getImageWidth mw
    h <- getImageHeight mw

    let
      -- Just convert the radians to degrees. This way I don't have
      -- to write a function which sets up an affine rotation for an
      -- argument specified in radians rather than degrees.
      -- You can always change this.
      angle_degrees = degrees(atan(realToFrac(h) / realToFrac(w)))
      -- Initialize the required affines
      -- Rotate diagonal to the x axis
      r1 = rotate_affine (-angle_degrees)
      -- Reflection affine (about x-axis)
      -- In this case there isn't a specific function to set the
      -- affine array (like there is for rotation and scaling)
      -- so use the function which sets an arbitrary affine
      reflect = [  1, 0, 0,
                  -1, 0, 0 ]
      -- rotate image back again
      r2 = rotate_affine angle_degrees
      -- temp = r1*reflect
      temp = r1 `affine_multiply` reflect
      -- result = temp*r2;
      result = temp `affine_multiply` r2

    distortImage mw affineProjectionDistortion result False

    writeImage mw (Just "logo_affine_3.jpg")

  localGenesis $ do
    -- Example 4
    -- Create a rotated gradient
    -- See: http:--www.imagemagick.org/discourse-server/viewtopic.php?f=1&t=12707
    -- The affine in this one is essentially the same as the one in Example 2 but
    -- this example has a different use for the result
    let
      -- Dimensions of the final rectangle
      w = 600 :: Int
      h = 100 :: Int
      -- angle of clockwise rotation
      theta = 15	-- degrees
      -- Convert theta to radians
      rad_theta = radians theta
      -- Compute the dimensions of the rectangular gradient
      -- Don't let the rotation make the gradient rectangle any smaller
      -- than the required output (using `max`)
      gw = max w $ round (fromIntegral w * cos rad_theta + fromIntegral h * sin rad_theta + 0.5)
      gh = max h $ round (fromIntegral w * sin rad_theta + fromIntegral h * cos rad_theta + 0.5)

    (_,mw) <- magickWand
    setSize mw gw gh
    readImage mw "gradient:white-black"

    let
      -- Initialize the required affines
      -- translate centre of gradient to origin
      t1 = translate_affine (- fromIntegral gw / 2) (- fromIntegral gh / 2)
      -- rotate clockwise by theta degrees
      r = rotate_affine(theta)
      -- translate back again
      t2 = translate_affine (fromIntegral gw / 2) (fromIntegral gh / 2)
      -- Now multiply the affine sequences
      -- temp = t1*r
      temp = t1 `affine_multiply` r
      -- result = temp*t2;
      result = temp `affine_multiply` t2

    distortImage mw affineProjectionDistortion result False
    -- Get the size of the distorted image and crop out the middle
    nw <- getImageWidth mw
    nh <- getImageHeight mw
    cropImage mw w h ((nw -  w) `div` 2) ((nh - h) `div` 2)
    writeImage mw (Just "rotgrad_2.png")



