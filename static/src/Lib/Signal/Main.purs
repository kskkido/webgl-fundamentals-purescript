module Lib.Signal.Main where

import Prelude
import Effect as Effect
import Signal as Signal
import Signal.Channel as Signal.Channel
import Data.Maybe as Maybe
import Web.Event.Event as Web.Event.Event
import Web.Event.EventTarget as Web.Event.EventTarget
import Web.Event.Internal.Types as Web.Event.Internal.Types

fromEvent :: Web.Event.Event.EventType -> Web.Event.EventTarget.EventTarget -> Effect.Effect (Signal.Signal (Maybe.Maybe Web.Event.Internal.Types.Event))
fromEvent ex tx = do
  channel  <- Signal.Channel.channel Maybe.Nothing
  listener <- Web.Event.EventTarget.eventListener (Signal.Channel.send channel <<< pure)
  Web.Event.EventTarget.addEventListener ex listener false tx
  pure $ Signal.Channel.subscribe channel

