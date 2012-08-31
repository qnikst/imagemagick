{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.ImageMagick.MagickWand.FFI.WandProperties
  where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickWand.FFI.Types


foreign import ccall "MagickDeleteOption" magickDeleteOption
  :: Ptr MagickWand
  -> CString        -- ^ the key
  -> IO MagickBooleanType

-- | MagickGetOption() returns a value associated with a wand
-- and the specified key. Use MagickRelinquishMemory() to free
-- the value when you are finished with it.
foreign import ccall "MagickGetOption" magickGetOption
  :: Ptr MagickWand
  -> CString        -- ^ the key
  -> IO CString

-- | MagickSetOption() associates one or options with the wand
-- (e.g. MagickSetOption(wand,"jpeg:perserve","yes")).
foreign import ccall "MagickSetOption" magickSetOption
  :: Ptr MagickWand
  -> CString        -- ^ the key
  -> CString        -- ^ the value
  -> IO MagickBooleanType

-- | MagickGetOptions() returns all the option names that match the
-- specified pattern associated with a wand. Use MagickGetOption()
-- to return the value of a particular option. Use MagickRelinquishMemory()
-- to free the value when you are finished with it.
foreign import ccall "MagickGetOptions" magickGetOptions
  :: Ptr MagickWand
  -> CString        -- ^ the pattern
  -> Ptr CSize
  -> IO (Ptr CString)

-- | MagickDeleteImageProperty() deletes a wand property.
foreign import ccall "MagickDeleteImageProperty" magickDeleteImageProperty
  :: Ptr MagickWand
  -> CString        -- ^ the property
  -> IO MagickBooleanType

-- | MagickGetImageProperty() returns a value associated with the
-- specified property. Use MagickRelinquishMemory() to free the value
-- when you are finished with it.
foreign import ccall "MagickGetImageProperty" magickGetImageProperty
  :: Ptr MagickWand
  -> CString        -- ^ the property
  -> IO CString

-- | MagickGetImageProperties() returns all the property names that
-- match the specified pattern associated with a wand. Use
-- MagickGetImageProperty() to return the value of a particular property.
-- Use MagickRelinquishMemory() to free the value when you are finished
-- with it.
foreign import ccall "MagickGetImageProperties" magickGetImageProperties
  :: Ptr MagickWand
  -> CString        -- ^ the pattern
  -> Ptr CSize
  -> IO (Ptr CString)

-- | MagickSetImageProperty() associates a property with an image.
foreign import ccall "MagickSetImageProperty" magickSetImageProperty
  :: Ptr MagickWand
  -> CString        -- ^ the property
  -> CString        -- ^ the value
  -> IO MagickBooleanType

-- | MagickGetImageProfile() returns the named image profile.
foreign import ccall "MagickGetImageProfile" magickGetImageProfile
  :: Ptr MagickWand
  -> CString        -- ^ the profile name
  -> Ptr CSize      -- ^ the profile length
  -> IO (Ptr Word8)

-- | MagickRemoveImageProfile() removes the named image profile and
-- returns it.
foreign import ccall "MagickRemoveImageProfile" magickRemoveImageProfile
  :: Ptr MagickWand
  -> CString        -- ^ the profile name
  -> Ptr CSize      -- ^ the profile length
  -> IO (Ptr Word8)

-- | MagickSetImageProfile() adds a named profile to the magick wand.
-- If a profile with the same name already exists, it is replaced.
-- This method differs from the MagickProfileImage() method in that
-- it does not apply any CMS color profiles.
foreign import ccall "MagickSetImageProfile" magickSetImageProfile
  :: Ptr MagickWand
  -> CString        -- ^ the profile name
  -> Ptr Word8      -- ^ the profile
  -> CSize          -- ^ the profile length
  -> IO MagickBooleanType

-- | MagickGetImageProfiles() returns all the profile names that match
-- the specified pattern associated with a wand. Use
-- MagickGetImageProfile() to return the value of a particular property.
-- Use MagickRelinquishMemory() to free the value when you are finished
-- with it.
foreign import ccall "MagickGetImageProfiles" magickGetImageProfiles
  :: Ptr MagickWand
  -> CString        -- ^ the pattern
  -> Ptr CSize
  -> IO (Ptr CString)
