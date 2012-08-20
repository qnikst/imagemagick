{-# LANGUAGE OverloadedStrings #-}
import           Data.Int
import           Control.Monad                   (when)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Resource    (release)
import           Graphics.ImageMagick.MagickWand
import           Text.Printf                     (printf)

throwAPIException w = undefined

exitWithMessage msg = undefined

iterateWand magick_wand = magickIterate magick_wand $ \w -> do
  i <- getIteratorIndex w
  s <- getImageScene w
  liftIO $ putStrLn $ printf "index %02d scene %02d" i s


main :: IO ()
main = withMagickWandGenesis $ do
  (_,magick_wand) <- magickWand
  setSize magick_wand 640 480
  size <- getSize magick_wand
  when (size /= (640,480)) $ exitWithMessage "Unexpected magick wand size\n"
  liftIO $ putStrLn "Reading images...\n"
  readImage magick_wand "sequence.miff"
  n <- getNumberImages magick_wand
  when (n /= 5) $ liftIO $ putStrLn $ printf "read %02d images; expected 5" n
  liftIO $ putStrLn "Iterate forward..."
  iterateWand magick_wand

  liftIO $ putStrLn "Iterate reverse..."
  magickIterateReverse magick_wand $ \w -> do
    i <- getIteratorIndex w
    s <- getImageScene w
    liftIO $ putStrLn $ printf "index %02d scene %02d" i s

  liftIO $ putStrLn "Remove scene 1..."
  setIteratorIndex magick_wand 1
  (clone_key,clone_wand) <- getImage magick_wand
  removeImage magick_wand
  iterateWand magick_wand

  liftIO $ putStrLn "Insert scene 1 back in sequence..."
  setIteratorIndex magick_wand 0
  addImage magick_wand clone_wand
  iterateWand magick_wand

  liftIO $ putStrLn "Set scene 2 to scene 1..."
  setIteratorIndex magick_wand 2
  setImage magick_wand clone_wand
  release clone_key
  iterateWand magick_wand

  liftIO $ putStrLn "Apply image processing options..."
  cropImage magick_wand 60 60 10 10
  resetIterator magick_wand
  background <- pixelWand
  background `setColor` "#000000"
--  rotateImage magick_wand background 90.0
  border <- pixelWand
  background `setColor` "green"
  border `setColor` "black"
  floodfillPaintImage magick_wand compositeChannels background
    (0.01*quantumRange) border 0 0 False

  (drawing_key,drawing_wand) <- drawingWand
  pushDrawingWand drawing_wand
--  drawRotate drawing_wand 45
  drawing_wand `setFontSize` 18
  fill <- pixelWand
  fill `setColor` "green"
  drawing_wand `setFillColor` fill
  -- ? fill=DestroyPixelWand(fill);
  drawAnnotation drawing_wand 15 5 "Magick"
  popDrawingWand drawing_wand
  setIteratorIndex magick_wand 1
  drawImage magick_wand drawing_wand
  annotateImage magick_wand drawing_wand 70 5 90 "Image"
  release drawing_key

  let primary_colors = [
          0,   0,   0,
          0,   0, 255,
          0, 255,   0,
          0, 255, 255,
        255, 255, 255,
        255,   0,   0,
        255,   0, 255,
        255, 255,   0,
        128, 128, 128
        ] :: [Int8]

  setIteratorIndex magick_wand 2
  importImagePixels magick_wand 10 10 3 3 "RGB" primary_colors
  pixels <- exportImagePixels magick_wand 10 10 3 3 "RGB"

  when (pixels /= primary_colors) $ exitWithMessage "Get pixels does not match set pixels"
{-
  {
    unsigned char
      pixels[27],
    (void) MagickSetIteratorIndex(magick_wand,2);
    status=MagickImportImagePixels(magick_wand,10,10,3,3,"RGB",CharPixel,
      primary_colors);
    if (status == MagickFalse)
      throwAPIException magick_wand);
    status=MagickExportImagePixels(magick_wand,10,10,3,3,"RGB",CharPixel,
      pixels);

    if (status == MagickFalse)
      throwAPIException magick_wand);
    for (i=0; i < 9; i++)
      if (pixels[i] != primary_colors[i])
        {
          (void) FormatLocaleFile(stderr,
            "Get pixels does not match set pixels\n");
          exit(1);
        }
  }



  (void) MagickSetIteratorIndex(magick_wand,3);
  status=MagickResizeImage(magick_wand,50,50,UndefinedFilter,1.0);
  if (status == MagickFalse)
    throwAPIException magick_wand);
  MagickResetIterator(magick_wand);
  while (MagickNextImage(magick_wand) != MagickFalse)
  {
    (void) MagickSetImageDepth(magick_wand,8);
    (void) MagickSetImageCompression(magick_wand,RLECompression);
  }
  MagickResetIterator(magick_wand);
  (void) MagickSetIteratorIndex(magick_wand,4);
  (void) FormatLocaleFile(stdout,
    "Utilitize pixel iterator to draw diagonal...\n");
  iterator=NewPixelIterator(magick_wand);
  if (iterator == (PixelIterator *) NULL)
    throwAPIException magick_wand);
  pixels=PixelGetNextIteratorRow(iterator,&number_wands);
  for (i=0; pixels != (PixelWand **) NULL; i++)
  {
    (void) PixelSetColor(pixels[i],"#224466");
    (void) PixelSyncIterator(iterator);
    pixels=PixelGetNextIteratorRow(iterator,&number_wands);
  }
  (void) PixelSyncIterator(iterator);
  iterator=DestroyPixelIterator(iterator);
  (void) FormatLocaleFile(stdout,"Write to wandtest_out.miff...\n");
  status=MagickWriteImages(magick_wand,"wandtest_out.miff",MagickTrue);
  if (status == MagickFalse)
    throwAPIException magick_wand);
  (void) FormatLocaleFile(stdout,
    "Change image format from \"MIFF\" to \"GIF\"...\n");
  status=MagickSetImageFormat(magick_wand,"GIF");
  if (status == MagickFalse)
     throwAPIException magick_wand);
  (void) FormatLocaleFile(stdout,"Set delay between frames to %d seconds...\n",
    WandDelay);
  status=MagickSetImageDelay(magick_wand,100*WandDelay);
  if (status == MagickFalse)
    throwAPIException magick_wand);
  delay=MagickGetImageDelay(magick_wand);
  if (delay != (100*WandDelay))
    {
      (void) FormatLocaleFile(stderr,"Get delay does not match set delay\n");
      exit(1);
    }
  (void) FormatLocaleFile(stdout,"Write to wandtest_out.gif...\n");
  status=MagickWriteImages(magick_wand,"wandtest_out.gif",MagickTrue);
  if (status == MagickFalse)
    throwAPIException magick_wand);
  (void) FormatLocaleFile(stdout,"Set, list, get, and delete wand option...\n");
  status=MagickSetOption(magick_wand,"wand:custom-option",CustomOption);
  if (status == MagickFalse)
    throwAPIException magick_wand);
  option=MagickGetOption(magick_wand,"wand:custom-option");
  if ((option == (const char *) NULL) ||
      (strlen(option) != strlen(CustomOption)) ||
      (memcmp(option,CustomOption,strlen(option)) != 0))
    {
      (void) FormatLocaleFile(stderr,"Option does not match\n");
      exit(1);
    }
  options=MagickGetOptions(magick_wand,"*",&number_options);
  if (options != (char **) NULL)
    {
      for (i=0; i < (ssize_t) number_options; i++)
      {
        (void) FormatLocaleFile(stdout,"  %s\n",options[i]);
        options[i]=(char *) MagickRelinquishMemory(options[i]);
      }
      options=(char **) MagickRelinquishMemory(options);
    }
  status=MagickDeleteOption(magick_wand,"wand:custom-option");
  if (status == MagickFalse)
    throwAPIException magick_wand);
  (void) FormatLocaleFile(stdout,
    "Set, list, get, and delete wand property...\n");
  status=MagickSetImageProperty(magick_wand,"wand:custom-property",
    CustomProperty);
  if (status == MagickFalse)
    throwAPIException magick_wand);
  property=MagickGetImageProperty(magick_wand,"wand:custom-property");
  if ((property == (const char *) NULL) ||
      (strlen(property) != strlen(CustomProperty)) ||
      (memcmp(property,CustomProperty,strlen(property)) != 0))
    {
      (void) FormatLocaleFile(stderr,"Property does not match\n");
      exit(1);
    }
  properties=MagickGetImageProperties(magick_wand,"*",&number_properties);
  if (properties != (char **) NULL)
    {
      for (i=0; i < (ssize_t) number_properties; i++)
      {
        (void) FormatLocaleFile(stdout,"  %s\n",properties[i]);
        properties[i]=(char *) MagickRelinquishMemory(properties[i]);
      }
      properties=(char **) MagickRelinquishMemory(properties);
    }
  status=MagickDeleteImageProperty(magick_wand,"wand:custom-property");
  if (status == MagickFalse)
    throwAPIException magick_wand);
  (void) FormatLocaleFile(stdout,
    "Set, list, get, and remove sRGB color profile...\n");
  status=MagickSetImageProfile(magick_wand,"sRGB",sRGBProfile,
    sizeof(sRGBProfile));
  if (status == MagickFalse)
    throwAPIException magick_wand);
  profile=(unsigned char *) MagickGetImageProfile(magick_wand,"sRGB",&length);
  if ((profile == (unsigned char *) NULL) || (length != sizeof(sRGBProfile)) ||
      (memcmp(profile,sRGBProfile,length) != 0))
    {
      (void) FormatLocaleFile(stderr,"Profile does not match\n");
      exit(1);
    }
  profile=(unsigned char *) MagickRelinquishMemory(profile);
  profiles=MagickGetImageProfiles(magick_wand,"*",&number_profiles);
  if (profiles != (char **) NULL)
    {
      for (i=0; i < (ssize_t) number_profiles; i++)
      {
        (void) FormatLocaleFile(stdout,"  %s\n",profiles[i]);
        profiles[i]=(char *) MagickRelinquishMemory(profiles[i]);
      }
      profiles=(char **) MagickRelinquishMemory(profiles);
    }
  profile=(unsigned char *) MagickRemoveImageProfile(magick_wand,"sRGB",
    &length);
  if ((profile == (unsigned char *) NULL) || (length != sizeof(sRGBProfile)) ||
      (memcmp(profile,sRGBProfile,length) != 0))
    {
      (void) FormatLocaleFile(stderr,"Profile does not match\n");
      exit(1);
    }
  profile=(unsigned char *) MagickRelinquishMemory(profile);
-}
  liftIO $ putStrLn "Wand tests pass."
