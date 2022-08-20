module Lib.Float32Array.Main where

import Prelude
import Effect as Effect
import Data.ArrayBuffer.Types as ArrayBuffer.Types

type Float32Array = ArrayBuffer.Types.Float32Array

foreign import fromArrayImpl :: Array Number -> Effect.Effect Float32Array

fromArray :: Array Number -> Effect.Effect Float32Array
fromArray = fromArrayImpl

foreign import toArrayBufferImpl :: Float32Array -> ArrayBuffer.Types.ArrayBuffer

toArrayBuffer :: Float32Array -> ArrayBuffer.Types.ArrayBuffer
toArrayBuffer = toArrayBufferImpl
