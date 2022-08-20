module Lib.Image.Main where

import Prelude
import Effect as Effect
import Lib.Window.Image.Main as Lib.Window.Image
import Lib.Image.Models.Dimension.Main as Models.Dimension

getDimension :: Lib.Window.Image.Image -> Effect.Effect Models.Dimension.Dimension
getDimension canvas = do
  width  <- Lib.Window.Image.getWidth canvas
  height <- Lib.Window.Image.getHeight canvas
  pure { width: width, height: height }
