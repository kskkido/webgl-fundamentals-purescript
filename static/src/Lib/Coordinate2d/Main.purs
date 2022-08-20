module Lib.Coordinate2d.Main where

import Prelude
import Math as Math
import Data.Tuple.Nested as Tuple.Nested
import Lib.Math.Main as Lib.Math

type Coordinate2d = Tuple.Nested.Tuple2 Number Number

fromRadian :: Number -> Coordinate2d
fromRadian n = Tuple.Nested.tuple2 (Math.cos n) (Math.sin n)

fromDegree :: Number -> Coordinate2d
fromDegree = fromRadian <<< Lib.Math.radian
