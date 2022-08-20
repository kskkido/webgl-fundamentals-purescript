module Lib.Graphics.Models.Rectangle.Main where

import Prelude
import Effect as Effect
import Effect.Exception as Effect.Exception
import Data.Maybe as Maybe
import Data.Float32 as Float32
import Data.Traversable as Traversable
import Data.ArrayBuffer.Typed as ArrayBuffer.Typed

type Rectangle =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

toArray :: Rectangle -> Array Number
toArray model =
  let x1 = model.x
      x2 = model.x + model.width
      y1 = model.y
      y2 = model.y + model.height
  in [x1, y1, x2, y1, x1, y2, x1, y2, x2, y1, x2, y2]

