module Lib.Window.Canvas.Main where

import Prelude
import Effect as Effect
import Data.Maybe as Maybe
import Graphics.Canvas as Graphics.Canvas
import Lib.Maybe.Main as Lib.Maybe
import Lib.Window.WebGL.Main as WebGL
import Lib.Window.WebGL.Types as WebGL.Types

foreign import getWebGLContextImpl :: Graphics.Canvas.CanvasElement -> Effect.Effect WebGL.Types.WebGLContext

getWebGLContext :: Graphics.Canvas.CanvasElement -> Effect.Effect (Maybe.Maybe WebGL.Types.WebGLContext)
getWebGLContext = getWebGLContextImpl >>> map Lib.Maybe.from

foreign import getClientWidthImpl :: Graphics.Canvas.CanvasElement -> Effect.Effect Number

getClientWidth :: Graphics.Canvas.CanvasElement -> Effect.Effect Number
getClientWidth = getClientWidthImpl

foreign import getClientHeightImpl :: Graphics.Canvas.CanvasElement -> Effect.Effect Number

getClientHeight :: Graphics.Canvas.CanvasElement -> Effect.Effect Number
getClientHeight = getClientHeightImpl

foreign import resizeImpl :: Graphics.Canvas.CanvasElement -> Effect.Effect Unit

resize :: Graphics.Canvas.CanvasElement -> Effect.Effect Unit
resize = resizeImpl

