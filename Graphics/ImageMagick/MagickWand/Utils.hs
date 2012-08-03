module Graphics.ImageMagick.MagickWand.Utils
  ( fromMBool
  )
  where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Graphics.ImageMagick.MagickWand.FFI.Types

fromMBool :: (MonadResource m) => IO MagickBooleanType -> m Bool
fromMBool = liftM (==mTrue) . liftIO
