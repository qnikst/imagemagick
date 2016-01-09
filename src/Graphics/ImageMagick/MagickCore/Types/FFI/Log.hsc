{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.ImageMagick.MagickCore.Types.FFI.Log
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>


newtype LogEventType = LogEventType { unLogEventType :: CInt }
          deriving (Eq, Show)

#{enum LogEventType, LogEventType
  , undefinedEvents = UndefinedEvents
  , noEvents = NoEvents
  , traceEvent = TraceEvent
  , annotateEvent = AnnotateEvent
  , blobEvent = BlobEvent
  , cacheEvent = CacheEvent
  , coderEvent = CoderEvent
  , configureEvent = ConfigureEvent
  , deprecateEvent = DeprecateEvent
  , drawEvent = DrawEvent
  , exceptionEvent = ExceptionEvent
  , imageEvent = ImageEvent
  , localeEvent = LocaleEvent
  , moduleEvent = ModuleEvent
  , policyEvent = PolicyEvent
  , resourceEvent = ResourceEvent
  , transformEvent = TransformEvent
  , userEvent = UserEvent
  , wandEvent = WandEvent
  , x11Event = X11Event
  , accelerateEvent = AccelerateEvent
  , allEvents = AllEvents
}
