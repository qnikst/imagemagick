module Graphics.ImageMagick.MagickCore.Quantize where

import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class

import           Foreign

import qualified Graphics.ImageMagick.MagickCore.FFI.Quantize as F
import           Graphics.ImageMagick.MagickCore.Types.FFI.Quantize (QuantizeInfo)

getQuantizeInfo :: (MonadResource m) => m QuantizeInfo
getQuantizeInfo = liftIO $
  alloca $ \qiPtr -> do
    () <- F.c_getQuantizeInfo qiPtr
    peek qiPtr
