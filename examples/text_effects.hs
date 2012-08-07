{-# LANGUAGE OverloadedStrings #-}
-- | Example taken from: http://members.shaw.ca/el.supremo/MagickWand/text_effects.htm
{- There's no equivalent convert command for this. It is a demo of MagickWand.
See this forum thread for the genesis of these effects
http://www.imagemagick.org/discourse-server/viewtopic.php?f=6&t=11586
and Anthony's Text Effects page at:
http://www.imagemagick.org/Usage/fonts/
-}

import           Control.Monad.Trans.Resource
import           Graphics.ImageMagick.MagickWand

{-
// Given a pattern name (which MUST have a leading #) and a pattern file,
// set up a pattern URL for later reference in the specified drawing wand
// Currently only used in Text Effect 2
void set_tile_pattern(DrawingWand *d_wand,char *pattern_name,char *pattern_file)
{
	MagickWand *t_wand;
	long w,h;

	t_wand=NewMagickWand();
	MagickReadImage(t_wand,pattern_file);
	// Read the tile's width and height
	w = MagickGetImageWidth(t_wand);
	h = MagickGetImageHeight(t_wand);

	DrawPushPattern(d_wand, pattern_name+1, 0, 0, w, h);
	DrawComposite(d_wand, SrcOverCompositeOp, 0, 0, 0, 0, t_wand);
	DrawPopPattern(d_wand);
	DrawSetFillPatternURL(d_wand, pattern_name);
}
-}

  -- Text effect 1 - shadow effect using MagickShadowImage
  -- This is derived from Anthony's Soft Shadow effect
  -- convert -size 300x100 xc:none -font Candice -pointsize 72 \
  --           -fill white  -stroke black  -annotate +25+65 'Anthony' \
  --           \( +clone -background navy  -shadow 70x4+5+5 \) +swap \
  --           -background lightblue -flatten  -trim +repage  font_shadow_soft.jpg

  -- NOTE - if an image has a transparent background, adding a border of any colour other
  -- than "none" will remove all the transparency and replace it with the border's colour
textEffect1 :: (MonadResource m) => PMagickWand -> PDrawingWand -> PPixelWand -> m ()
textEffect1 w dw pw = do
  pw `setColor` "none"
  -- Create a new transparent image
  newImage w 350 100 pw
  -- Set up a 72 point white font
  pw `setColor` "white"
  dw `setFillColor` pw
  dw `setFont` "Verdana-Bold-Italic"
  dw `setFontSize` 72
  -- Add a black outline to the text
  pw `setColor` "black"
  dw `setStrokeColor` pw
  -- Turn antialias on - not sure this makes a difference
  dw `setTextAntialias` True
  -- Now draw the text
  drawAnnotation dw 25 65 "Magick"
  -- Draw the image on to the magick_wand
  drawImage w dw

  -- Trim the image down to include only the text
  trimImage w 0
  -- equivalent to the command line +repage
  resetImagePage w ""

  -- Make a copy of the text image
  (_,cloneW) <- cloneMagickWand w
  -- Set the background colour to blue for the shadow
  pw `setColor` "blue"
  w `setImageBackgroundColor` pw

  -- Opacity is a real number indicating (apparently) percentage
  shadowImage w 70 4 5 5

  -- Composite the text on top of the shadow
  compositeImage w cloneW overCompositeOp 5 5

  (_,w') <- magickWand
  -- Create a new image the same size as the text image and put a solid colour
  -- as its background
  pw `setColor` "rgb(125,215,255)"
  width <- getImageWidth w
  height <- getImageHeight w
  newImage w' width height pw
  -- Now composite the shadowed text over the plain background
  compositeImage w' w overCompositeOp 0 0
  --  and write the result
  writeImage cloneW (Just "text_shadow.png")

-- Text effect 2 - tiled text using the builtin checkerboard pattern
-- Anthony's Tiled Font effect
-- convert -size 320x100 xc:lightblue -font Candice -pointsize 72 \
--          -tile pattern:checkerboard   -annotate +28+68 'Anthony' \
--           font_tile.jpg
textEffect2 :: (MonadResource m) => PMagickWand -> PDrawingWand -> PPixelWand -> m ()
textEffect2 w dw pw = do
{-
	MagickWand *magick_wand = NULL;
	MagickWand *c_wand = NULL;
	DrawingWand *d_wand = NULL;
	PixelWand *p_wand = NULL;

	// Used for text effect #3
	double dargs[1] = {120.};

	// Used for text effect #5
	double d_args[8] = {
		-0.02,0.0,
		0.0,1.02,
		0.0,0.0,
		-0.5,1.9
	};

	MagickWandGenesis();

---------------

	/* Clean up */
	if(magick_wand)magick_wand = DestroyMagickWand(magick_wand);
	if(c_wand)c_wand = DestroyMagickWand(c_wand);
	if(d_wand)d_wand = DestroyDrawingWand(d_wand);
	if(p_wand)p_wand = DestroyPixelWand(p_wand);

--------------
	magick_wand = NewMagickWand();
	d_wand = NewDrawingWand();
	p_wand = NewPixelWand();

	set_tile_pattern(d_wand,"#check","pattern:checkerboard");

	PixelSetColor(p_wand,"lightblue");
	// Create a new transparent image
	MagickNewImage(magick_wand,320,100,p_wand);

	// Set up a 72 point font
	DrawSetFont (d_wand, "Verdana-Bold-Italic" ) ;
	DrawSetFontSize(d_wand,72);
	// Now draw the text
	DrawAnnotation(d_wand,28,68,"Magick");
	// Draw the image on to the magick_wand
	MagickDrawImage(magick_wand,d_wand);
	// Trim the image
	MagickTrimImage(magick_wand,0);
	// Add a transparent border
	PixelSetColor(p_wand,"lightblue");
	MagickBorderImage(magick_wand,p_wand,5,5);
	// and write it
	MagickWriteImage(magick_wand,"text_pattern.png");

	/* Clean up */
	if(magick_wand)magick_wand = DestroyMagickWand(magick_wand);
	if(d_wand)d_wand = DestroyDrawingWand(d_wand);
	if(p_wand)p_wand = DestroyPixelWand(p_wand);

// Text effect 3 -  arc font (similar to http://www.imagemagick.org/Usage/fonts/#arc)
//convert -size 320x100 xc:lightblue -font Candice -pointsize 72 \
//           -annotate +25+65 'Anthony' -distort Arc 120 \
//           -trim +repage -bordercolor lightblue -border 10  font_arc.jpg
	magick_wand = NewMagickWand();
	d_wand = NewDrawingWand();
	p_wand = NewPixelWand();

	// Create a 320x100 lightblue canvas
	PixelSetColor(p_wand,"lightblue");
	MagickNewImage(magick_wand,320,100,p_wand);

	// Set up a 72 point font
	DrawSetFont (d_wand, "Verdana-Bold-Italic" ) ;
	DrawSetFontSize(d_wand,72);
	// Now draw the text
	DrawAnnotation(d_wand,25,65,"Magick");
	// Draw the image on to the magick_wand
	MagickDrawImage(magick_wand,d_wand);

	MagickDistortImage(magick_wand,ArcDistortion,1,dargs,MagickFalse);
	// Trim the image
	MagickTrimImage(magick_wand,0);
	// Add the border
	PixelSetColor(p_wand,"lightblue");
	MagickBorderImage(magick_wand,p_wand,10,10);

	// and write it
	MagickWriteImage(magick_wand,"text_arc.png");

	/* Clean up */
	if(magick_wand)magick_wand = DestroyMagickWand(magick_wand);
	if(d_wand)d_wand = DestroyDrawingWand(d_wand);
	if(p_wand)p_wand = DestroyPixelWand(p_wand);

// Text effect 4 - bevelled font http://www.imagemagick.org/Usage/fonts/#bevel
// convert -size 320x100 xc:black -font Candice -pointsize 72 \
//              -fill white   -annotate +25+65 'Anthony' \
//              -shade 140x60  font_beveled.jpg
	magick_wand = NewMagickWand();
	d_wand = NewDrawingWand();
	p_wand = NewPixelWand();

	// Create a 320x100 canvas
	PixelSetColor(p_wand,"gray");
	MagickNewImage(magick_wand,320,100,p_wand);
	// Set up a 72 point font
	DrawSetFont (d_wand, "Verdana-Bold-Italic" ) ;
	DrawSetFontSize(d_wand,72);
	// Set up a 72 point white font
	PixelSetColor(p_wand,"white");
	DrawSetFillColor(d_wand,p_wand);
	// Now draw the text
	DrawAnnotation(d_wand,25,65,"Magick");
	// Draw the image on to the magick_wand
	MagickDrawImage(magick_wand,d_wand);
	// the "gray" parameter must be true to get the effect shown on Anthony's page
	MagickShadeImage(magick_wand,MagickTrue,140,60);

#ifdef COLORIZE
	PixelSetColor(p_wand,"yellow");
	DrawSetFillColor(d_wand,p_wand);
	cp_wand = NewPixelWand();
	PixelSetColor(cp_wand,"gold");
	MagickColorizeImage(magick_wand,p_wand,cp_wand);
#endif
	// and write it
	MagickWriteImage(magick_wand,"text_bevel.png");

	/* Clean up */
	if(magick_wand)magick_wand = DestroyMagickWand(magick_wand);
	if(d_wand)d_wand = DestroyDrawingWand(d_wand);
	if(p_wand)p_wand = DestroyPixelWand(p_wand);
#ifdef COLORIZE
	if(cp_wand)cp_wand = DestroyPixelWand(cp_wand);
#endif


// Text effect 5 and 6 - Plain text and then Barrel distortion
	// This one uses d_args
	magick_wand = NewMagickWand();
	d_wand = NewDrawingWand();
	p_wand = NewPixelWand();

	// Create a 320x100 transparent canvas
	PixelSetColor(p_wand,"none");
	MagickNewImage(magick_wand,320,100,p_wand);

	// Set up a 72 point font
	DrawSetFont (d_wand, "Verdana-Bold-Italic" ) ;
	DrawSetFontSize(d_wand,72);
	// Now draw the text
	DrawAnnotation(d_wand,25,65,"Magick");
	// Draw the image on to the magick_wand
	MagickDrawImage(magick_wand,d_wand);
	MagickWriteImage(magick_wand,"text_plain.png");

	// Trim the image
	MagickTrimImage(magick_wand,0);
	// Add the border
	PixelSetColor(p_wand,"none");
	MagickBorderImage(magick_wand,p_wand,10,10);
//	MagickSetImageMatte(magick_wand,MagickTrue);
//	MagickSetImageVirtualPixelMethod(magick_wand,TransparentVirtualPixelMethod);
// 	d_args[0] = 0.1;d_args[1] = -0.25;d_args[2] = -0.25; [3] += .1
	// The first value should be positive. If it is negative the image is *really* distorted
	d_args[0] = 0.0;
	d_args[1] = 0.0;
	d_args[2] = 0.5;
	// d_args[3] should normally be chosen such the sum of all 4 values is 1
	// so that the result is the same size as the original
	// You can override the sum with a different value
	// If the sum is greater than 1 the resulting image will be smaller than the original
	d_args[3] = 1 - (d_args[0] + d_args[1] + d_args[2]);
	// Make the result image smaller so that it isn't as likely
	// to overflow the edges
	// d_args[3] += 0.1;
	// 0.0,0.0,0.5,0.5,0.0,0.0,-0.5,1.9
	d_args[3] = 0.5;
	d_args[4] = 0.0;
	d_args[5] = 0.0;
	d_args[6] = -0.5;
	d_args[7] = 1.9;
	// DON'T FORGET to set the correct number of arguments here
	MagickDistortImage(magick_wand,BarrelDistortion,8,d_args,MagickTrue);
//	MagickResetImagePage(magick_wand,"");
	// Trim the image again
	MagickTrimImage(magick_wand,0);
	// Add the border
	PixelSetColor(p_wand,"none");
	MagickBorderImage(magick_wand,p_wand,10,10);
	// and write it
	MagickWriteImage(magick_wand,"text_barrel.png");

	/* Clean up */
	if(magick_wand)magick_wand = DestroyMagickWand(magick_wand);
	if(d_wand)d_wand = DestroyDrawingWand(d_wand);
	if(p_wand)p_wand = DestroyPixelWand(p_wand);

// Text effect 7 - Polar distortion
	// This one uses d_args[0]
	magick_wand = NewMagickWand();
	d_wand = NewDrawingWand();
	p_wand = NewPixelWand();

	// Create a 320x200 transparent canvas
	PixelSetColor(p_wand,"none");
	MagickNewImage(magick_wand,320,200,p_wand);

	// Set up a 72 point font
	DrawSetFont (d_wand, "Verdana-Bold-Italic" ) ;
	DrawSetFontSize(d_wand,72);
	// Now draw the text
	DrawAnnotation(d_wand,25,65,"Magick");
	// Draw the image on to the magick_wand
	MagickDrawImage(magick_wand,d_wand);

	d_args[0] = 0.0;

	// DON'T FORGET to set the correct number of arguments here
	MagickDistortImage(magick_wand,PolarDistortion,1,d_args,MagickTrue);
//	MagickResetImagePage(magick_wand,"");
	// Trim the image again
	MagickTrimImage(magick_wand,0);
	// Add the border
	PixelSetColor(p_wand,"none");
	MagickBorderImage(magick_wand,p_wand,10,10);
	// and write it
	MagickWriteImage(magick_wand,"text_polar.png");

	/* Clean up */
	if(magick_wand)magick_wand = DestroyMagickWand(magick_wand);
	if(d_wand)d_wand = DestroyDrawingWand(d_wand);
	if(p_wand)p_wand = DestroyPixelWand(p_wand);

// Text effect 8 - Shepard's distortion
	// This one uses d_args[0]
	magick_wand = NewMagickWand();
	d_wand = NewDrawingWand();
	p_wand = NewPixelWand();

	// Create a 320x200 transparent canvas
	PixelSetColor(p_wand,"none");
	MagickNewImage(magick_wand,640,480,p_wand);

	// Set up a 72 point font
	DrawSetFont (d_wand, "Verdana-Bold-Italic" ) ;
	DrawSetFontSize(d_wand,72);
	// Now draw the text
	DrawAnnotation(d_wand,50,240,"Magick Rocks");
	// Draw the image on to the magick_wand
	MagickDrawImage(magick_wand,d_wand);
	d_args[0] = 150.0;
	d_args[1] = 190.0;
	d_args[2] = 100.0;
	d_args[3] = 290.0;
	d_args[4] = 500.0;
	d_args[5] = 200.0;
	d_args[6] = 430.0;
	d_args[7] = 130.0;
	// DON'T FORGET to set the correct number of arguments here
	MagickDistortImage(magick_wand,ShepardsDistortion,8,d_args,MagickTrue);

	// Trim the image
	MagickTrimImage(magick_wand,0);
	// Add the border
	PixelSetColor(p_wand,"none");
	MagickBorderImage(magick_wand,p_wand,10,10);
-}
  -- and write it
  writeImage w (Just "text_shepards.png")

runEffect e = localGenesis $ do
  (_,w) <- magickWand
  (_,dw) <- drawingWand
  pw <- pixelWand
  e w dw pw

main :: IO ()
main = withMagickWandGenesis $ do
  runEffect textEffect1
  runEffect textEffect2
