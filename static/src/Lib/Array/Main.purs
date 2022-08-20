module Lib.Array.Main where

import Prelude
import Data.Maybe as Maybe
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Foldable as Foldable

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose xxs = case Array.uncons xxs of
  Maybe.Nothing ->
    []
  Maybe.Just tx ->
    ( Array.range 0 (Array.length tx.head - 1) #
      map \i -> flip Array.index i `Array.mapMaybe` xxs
    )

revolving :: forall a. (a -> a -> a) -> a -> Array a -> Array a
revolving fn seed xs =
  ( Array.NonEmpty.tail $
    Foldable.foldl
      (\acc x -> Array.NonEmpty.snoc acc $ fn (Array.NonEmpty.last acc) x)
      (Array.NonEmpty.singleton seed)
      xs
  )
