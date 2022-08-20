module Lib.Uint8Array.Main where

import Prelude
import Effect as Effect
import Data.ArrayBuffer.Types as ArrayBuffer.Types

type Uint8Array = ArrayBuffer.Types.Uint8Array

foreign import fromArrayImpl :: Array Number -> Effect.Effect Uint8Array

fromArray :: Array Number -> Effect.Effect Uint8Array
fromArray = fromArrayImpl

foreign import toArrayBufferImpl :: Uint8Array -> ArrayBuffer.Types.ArrayBuffer

toArrayBuffer :: Uint8Array -> ArrayBuffer.Types.ArrayBuffer
toArrayBuffer = toArrayBufferImpl
