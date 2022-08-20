module Lib.Maybe.Main where

import Prelude
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe (..))

foreign import fromImpl :: forall a. Fn3 (Maybe a) (a -> Maybe a) a (Maybe a)

from :: forall a. a -> Maybe a
from x = runFn3 fromImpl Nothing Just x

