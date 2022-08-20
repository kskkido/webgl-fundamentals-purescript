module Lib.Math.Main where

import Prelude
import Math as Math

clamp :: Number -> Number -> Number -> Number
clamp x min max =
  if max >= min
  then Math.min (Math.max x min) max
  else clamp x max min

radian :: Number -> Number
radian n = n * (Math.pi / 180.0)

degree :: Number -> Number
degree n = n * 180.0 / Math.pi
