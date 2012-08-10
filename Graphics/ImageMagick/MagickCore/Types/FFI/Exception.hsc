{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.ImageMagick.MagickCore.Types.FFI.Exception
    where

import           Foreign.Storable
import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype ExceptionType = ExceptionType { unExceptionType :: CInt }
                      deriving (Eq,Show,Storable)

#{enum ExceptionType, ExceptionType
  ,  undefinedException =   UndefinedException
  ,  warningException  =   WarningException 
  ,  resourceLimitWarning  =   ResourceLimitWarning 
  ,  typeWarning  =   TypeWarning 
  ,  optionWarning  =   OptionWarning 
  ,  delegateWarning  =   DelegateWarning 
  ,  missingDelegateWarning  =   MissingDelegateWarning 
  ,  corruptImageWarning  =   CorruptImageWarning 
  ,  fileOpenWarning  =   FileOpenWarning 
  ,  blobWarning  =   BlobWarning 
  ,  streamWarning  =   StreamWarning 
  ,  cacheWarning  =   CacheWarning 
  ,  coderWarning  =   CoderWarning 
  ,  filterWarning  =   FilterWarning 
  ,  moduleWarning  =   ModuleWarning 
  ,  drawWarning  =   DrawWarning 
  ,  imageWarning  =   ImageWarning 
  ,  wandWarning  =   WandWarning 
  ,  randomWarning  =   RandomWarning 
  ,  xServerWarning  =   XServerWarning 
  ,  monitorWarning  =   MonitorWarning 
  ,  registryWarning  =   RegistryWarning 
  ,  configureWarning  =   ConfigureWarning 
  ,  policyWarning  =   PolicyWarning 
  ,  errorException  =   ErrorException 
  ,  resourceLimitError  =   ResourceLimitError 
  ,  typeError  =   TypeError 
  ,  optionError  =   OptionError 
  ,  delegateError  =   DelegateError 
  ,  missingDelegateError  =   MissingDelegateError 
  ,  corruptImageError  =   CorruptImageError 
  ,  fileOpenError  =   FileOpenError 
  ,  blobError  =   BlobError 
  ,  streamError  =   StreamError 
  ,  cacheError  =   CacheError 
  ,  coderError  =   CoderError 
  ,  filterError  =   FilterError 
  ,  moduleError  =   ModuleError 
  ,  drawError  =   DrawError 
  ,  imageError  =   ImageError 
  ,  wandError  =   WandError 
  ,  randomError  =   RandomError 
  ,  xServerError  =   XServerError 
  ,  monitorError  =   MonitorError 
  ,  registryError  =   RegistryError 
  ,  configureError  =   ConfigureError 
  ,  policyError  =   PolicyError 
  ,  fatalErrorException  =   FatalErrorException 
  ,  resourceLimitFatalError  =   ResourceLimitFatalError 
  ,  typeFatalError  =   TypeFatalError 
  ,  optionFatalError  =   OptionFatalError 
  ,  delegateFatalError  =   DelegateFatalError 
  ,  missingDelegateFatalError  =   MissingDelegateFatalError 
  ,  corruptImageFatalError  =   CorruptImageFatalError 
  ,  fileOpenFatalError  =   FileOpenFatalError 
  ,  blobFatalError  =   BlobFatalError 
  ,  streamFatalError  =   StreamFatalError 
  ,  cacheFatalError  =   CacheFatalError 
  ,  coderFatalError  =   CoderFatalError 
  ,  filterFatalError  =   FilterFatalError 
  ,  moduleFatalError  =   ModuleFatalError 
  ,  drawFatalError  =   DrawFatalError 
  ,  imageFatalError  =   ImageFatalError 
  ,  wandFatalError  =   WandFatalError 
  ,  randomFatalError  =   RandomFatalError 
  ,  xServerFatalError  =   XServerFatalError 
  ,  monitorFatalError  =   MonitorFatalError 
  ,  registryFatalError  =   RegistryFatalError 
  ,  configureFatalError  =   ConfigureFatalError 
  ,  policyFatalError  =   PolicyFatalError 

}
