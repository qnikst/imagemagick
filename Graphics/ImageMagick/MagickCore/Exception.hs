{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.ImageMagick.MagickCore.Exception
  ( ImageWandException(..)
  -- * support for ImageMagick Exceptions
  , ExceptionCarrier(..)
  ) where

import           Control.Exception.Base
import           Data.Typeable
import           Foreign
import           Foreign.C.String
import           Graphics.ImageMagick.MagickCore.Types

data ImageWandException = ImageWandException ExceptionSeverity ExceptionType String
  deriving (Typeable)


instance Show (ImageWandException) where
  show (ImageWandException _ x s) = concat [show x, ": ", s]

instance Exception ImageWandException

-- * Exception Carrier can be different objects
-- that are used in functions

class ExceptionCarrier a where
  getException :: a -> IO ImageWandException

