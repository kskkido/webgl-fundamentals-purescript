module Lib.Window.Main where

import Prelude
import Effect as Effect
import Web.HTML.Window as Web.HTML.Window

foreign import requestAnimationFrameImpl :: (Number -> Effect.Effect Unit) -> Web.HTML.Window.Window -> Effect.Effect Web.HTML.Window.RequestAnimationFrameId

requestAnimationFrame :: (Number -> Effect.Effect Unit) -> Web.HTML.Window.Window -> Effect.Effect Web.HTML.Window.RequestAnimationFrameId
requestAnimationFrame = requestAnimationFrameImpl
