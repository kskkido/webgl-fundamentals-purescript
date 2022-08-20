module Lib.Matrix4.Main where

import Prelude
import Math as Math
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Tuple.Nested as Tuple.Nested
import Data.Traversable as Traversable
import Lib.Matrix.Main as Lib.Matrix
import Lib.Math.Main as Lib.Math
import Lib.Vector3.Main as Lib.Vector3

unit :: Lib.Matrix.Matrix
unit =
  [ [1.0, 0.0, 0.0, 0.0]
  , [0.0, 1.0, 0.0, 0.0]
  , [0.0, 0.0, 1.0, 0.0]
  , [0.0, 0.0, 0.0, 1.0]
  ]

rotateX :: Tuple.Nested.Tuple2 Number Number -> Lib.Matrix.Matrix
rotateX tx =
  let
    x = Tuple.Nested.get1 tx
    y = Tuple.Nested.get2 tx
  in
    [ [1.0, 0.0, 0.0, 0.0]
    , [0.0, x, -y, 0.0]
    , [0.0, y, x, 0.0]
    , [0.0, 0.0, 0.0, 1.0]
    ]

rotateY :: Tuple.Nested.Tuple2 Number Number -> Lib.Matrix.Matrix
rotateY tx =
  let
    x = Tuple.Nested.get1 tx
    y = Tuple.Nested.get2 tx
  in
    [ [x, 0.0, y, 0.0]
    , [0.0, 1.0, 0.0, 0.0]
    , [-y, 0.0, x, 0.0]
    , [0.0, 0.0, 0.0, 1.0]
    ]

rotateZ :: Tuple.Nested.Tuple2 Number Number -> Lib.Matrix.Matrix
rotateZ tx =
  let
    x = Tuple.Nested.get1 tx
    y = Tuple.Nested.get2 tx
  in
    [ [x, -y, 0.0, 0.0]
    , [y, x, 0.0, 0.0]
    , [0.0, 0.0, 1.0, 0.0]
    , [0.0, 0.0, 0.0, 1.0]
    ]

translate :: Tuple.Nested.Tuple3 Number Number Number -> Lib.Matrix.Matrix
translate tx =
  let
    x = Tuple.Nested.get1 tx
    y = Tuple.Nested.get2 tx
    z = Tuple.Nested.get3 tx
  in
    [ [1.0, 0.0, 0.0, x]
    , [0.0, 1.0, 0.0, y]
    , [0.0, 0.0, 1.0, z]
    , [0.0, 0.0, 0.0, 1.0]
    ]

scale :: Tuple.Nested.Tuple3 Number Number Number -> Lib.Matrix.Matrix
scale tx =
  let
    x = Tuple.Nested.get1 tx
    y = Tuple.Nested.get2 tx
    z = Tuple.Nested.get3 tx
  in
    [ [x, 0.0, 0.0, 0.0]
    , [0.0, y, 0.0, 0.0]
    , [0.0, 0.0, z, 0.0]
    , [0.0, 0.0, 0.0, 1.0]
    ]

fudge :: Number -> Lib.Matrix.Matrix
fudge n =
  [ [1.0, 0.0, 0.0, 0.0]
  , [0.0, 1.0, 0.0, 0.0]
  , [0.0, 0.0, 1.0, 0.0]
  , [0.0, 0.0, n, 1.0]
  ]

projection :: Tuple.Nested.Tuple3 Number Number Number -> Lib.Matrix.Matrix
projection tx =
  let
    x = Tuple.Nested.get1 tx
    y = Tuple.Nested.get2 tx
    z = Tuple.Nested.get3 tx
  in
    [ [2.0 / x, 0.0, 0.0, -1.0]
    , [0.0, -2.0 / y, 0.0, 1.0]
    , [0.0, 0.0, 2.0 / z, 0.0]
    , [0.0, 0.0, 0.0, 1.0]
    ]

perspective :: Tuple.Nested.Tuple4 Number Number Number Number -> Lib.Matrix.Matrix
perspective tx =
  let
    fov = Lib.Math.radian $ Tuple.Nested.get1 tx
    a = Tuple.Nested.get2 tx
    x = Tuple.Nested.get3 tx
    y = Tuple.Nested.get4 tx
    f = Math.tan (Math.pi * 0.5 - 0.5 * fov)
    r = 1.0 / (x - y)
  in
    [ [f / a, 0.0, 0.0, 0.0]
    , [0.0, f, 0.0, 0.0]
    , [0.0, 0.0, (x + y) * r, x * y * r * 2.0]
    , [0.0, 0.0, -1.0, 0.0]
    ]

lookAt :: Lib.Vector3.Vector3 -> Lib.Vector3.Vector3 -> Lib.Vector3.Vector3 -> Lib.Matrix.Matrix
lookAt position target up =
  let zAxis = Lib.Vector3.normalize $ Lib.Vector3.subtract position target
      xAxis = Lib.Vector3.normalize $ Lib.Vector3.cross up zAxis
      yAxis = Lib.Vector3.normalize $ Lib.Vector3.cross zAxis xAxis
  in
    [ [Tuple.Nested.get1 xAxis, Tuple.Nested.get1 yAxis, Tuple.Nested.get1 zAxis, Tuple.Nested.get1 position]
    , [Tuple.Nested.get2 xAxis, Tuple.Nested.get2 yAxis, Tuple.Nested.get2 zAxis, Tuple.Nested.get2 position]
    , [Tuple.Nested.get3 xAxis, Tuple.Nested.get3 yAxis, Tuple.Nested.get3 zAxis, Tuple.Nested.get3 position]
    , [0.0, 0.0, 0.0, 1.0]
    ]

toCameraPosition :: Lib.Matrix.Matrix -> Lib.Vector3.Vector3
toCameraPosition mx = Maybe.maybe Lib.Vector3.unit identity $ do
  r <- Traversable.traverse Array.last mx
  Tuple.Nested.tuple3 <$> Array.index r 0 <*> Array.index r 1 <*> Array.index r 2
