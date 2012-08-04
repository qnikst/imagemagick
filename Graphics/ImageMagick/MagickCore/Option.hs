module Graphics.ImageMagick.MagickCore.Option
       ( parseChannelOption
       ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                            (ByteString, useAsCString)

import qualified Graphics.ImageMagick.MagickCore.FFI.Option as F
import           Graphics.ImageMagick.MagickWand.FFI.Types


parseChannelOption :: (MonadResource m) => ByteString -> m ChannelType
parseChannelOption s = liftIO $ useAsCString s F.parseChannelOption
