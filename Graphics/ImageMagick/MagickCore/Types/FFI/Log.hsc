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
  , oEvents = NoEvents
  , raceEvent = TraceEvent
  , nnotateEvent = AnnotateEvent
  , lobEvent = BlobEvent
  , acheEvent = CacheEvent
  , oderEvent = CoderEvent
  , onfigureEvent = ConfigureEvent
  , eprecateEvent = DeprecateEvent
  , rawEvent = DrawEvent
  , xceptionEvent = ExceptionEvent
  , mageEvent = ImageEvent
  , ocaleEvent = LocaleEvent
  , oduleEvent = ModuleEvent
  , olicyEvent = PolicyEvent
  , esourceEvent = ResourceEvent
  , ransformEvent = TransformEvent
  , serEvent = UserEvent
  , andEvent = WandEvent
  , x11Event = X11Event
  , ccelerateEvent = AccelerateEvent
  , allEvents = AllEvents
}
