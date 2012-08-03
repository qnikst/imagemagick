module Graphics.ImageMagick.MagickWand.Utils
  ( fromMBool
  , toMBool
  )
  where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Graphics.ImageMagick.MagickWand.FFI.Types

fromMBool :: (MonadResource m) => IO MagickBooleanType -> m Bool
fromMBool = liftM (==mTrue) . liftIO
{-# INLINE fromMBool #-}

toMBool :: Bool -> MagickBooleanType
toMBool True  = mTrue
toMBool False = mFalse
{-# INLINE toMBool #-}
