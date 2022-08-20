module Lib.Canvas.Main where

import Prelude
import Effect as Effect
import Graphics.Canvas as Graphics.Canvas
import Lib.Window.Canvas.Main as Lib.Window.Canvas
import Lib.Canvas.Models.Dimension.Main as Models.Dimension

getDimension :: Graphics.Canvas.CanvasElement -> Effect.Effect Models.Dimension.Dimension
getDimension canvas = do
  width  <- Lib.Window.Canvas.getClientWidth canvas
  height <- Lib.Window.Canvas.getClientHeight canvas
  pure { width: width, height: height }
