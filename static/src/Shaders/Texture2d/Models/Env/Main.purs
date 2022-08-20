module Shaders.Texture2d.Models.Env.Main where

import Prelude
import Graphics.Canvas as Graphics.Canvas
import Lib.Window.Image.Main as Lib.Window.Image
import Lib.Kernel.Main as Lib.Kernel

type Env =
  { canvas :: Graphics.Canvas.CanvasElement
  , image  :: Lib.Window.Image.Image
  , kernel :: Lib.Kernel.Kernel
  }
