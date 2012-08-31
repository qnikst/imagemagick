{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.ImageMagick.MagickCore.Exception
  ( MagickWandException(..)
  -- * support for ImageMagick Exceptions
  , ExceptionCarrier(..)
  ) where

import           Control.Exception.Base
import           Data.Typeable
import           Foreign
import           Foreign.C.String
import           Graphics.ImageMagick.MagickCore.Types

data MagickWandException = MagickWandException ExceptionSeverity ExceptionType String
  deriving (Typeable)


instance Show (MagickWandException) where
  show (MagickWandException _ x s) = concat [show x, ": ", s]

instance Exception MagickWandException

-- * Exception Carrier can be different objects
-- that are used in functions

class ExceptionCarrier a where
  getException :: a -> IO MagickWandException

