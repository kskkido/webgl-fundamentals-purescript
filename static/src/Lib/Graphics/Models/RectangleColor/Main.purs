module Lib.Graphics.Models.RectangleColor.Main where

import Prelude
import Effect as Effect
import Effect.Random as Effect.Random
import Data.Traversable as Traversable

type RectangleColor = Array Number

fromRandom :: Effect.Effect RectangleColor
fromRandom = do
  Traversable.sequence
    [ Effect.Random.random
    , Effect.Random.random
    , Effect.Random.random
    , pure 1.0
    , Effect.Random.random
    , Effect.Random.random
    , Effect.Random.random
    , pure 1.0
    , Effect.Random.random
    , Effect.Random.random
    , Effect.Random.random
    , pure 1.0
    , Effect.Random.random
    , Effect.Random.random
    , Effect.Random.random
    , pure 1.0
    , Effect.Random.random
    , Effect.Random.random
    , Effect.Random.random
    , pure 1.0
    , Effect.Random.random
    , Effect.Random.random
    , Effect.Random.random
    , pure 1.0
    ]

