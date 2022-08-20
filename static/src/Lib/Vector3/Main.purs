module Lib.Vector3.Main where

import Prelude
import Math as Math
import Data.Maybe as Maybe
import Data.Tuple.Nested as Tuple.Nested
import Control.MonadPlus as Control.MonadPlus

type Vector3 = Tuple.Nested.Tuple3 Number Number Number

unit :: Vector3
unit = Tuple.Nested.tuple3 0.0 0.0 0.0

cross :: Vector3 -> Vector3 -> Vector3
cross vx vy =
  let x1 = Tuple.Nested.get1 vx
      y1 = Tuple.Nested.get2 vx
      z1 = Tuple.Nested.get3 vx
      x2 = Tuple.Nested.get1 vy
      y2 = Tuple.Nested.get2 vy
      z2 = Tuple.Nested.get3 vy
  in Tuple.Nested.tuple3
       (y1 * z2 - z1 * y2)
       (z1 * x2 - x1 * z2)
       (x1 * y2 - y1 * x2)

subtract :: Vector3 -> Vector3 -> Vector3
subtract vx vy = Tuple.Nested.tuple3
  (Tuple.Nested.get1 vx - Tuple.Nested.get1 vy)
  (Tuple.Nested.get2 vx - Tuple.Nested.get2 vy)
  (Tuple.Nested.get3 vx - Tuple.Nested.get3 vy)

normalize :: Vector3 -> Vector3
normalize vx = Maybe.maybe unit identity $ do
  let lx = length vx
  Control.MonadPlus.guard (lx > 0.000001)
  pure $ Tuple.Nested.tuple3
    (Tuple.Nested.get1 vx / lx)
    (Tuple.Nested.get2 vx / lx)
    (Tuple.Nested.get3 vx / lx)

length :: Vector3 -> Number
length vx =
  let x = Tuple.Nested.get1 vx
      y = Tuple.Nested.get2 vx
      z = Tuple.Nested.get3 vx
  in Math.sqrt (x * x + y * y + z * z)
