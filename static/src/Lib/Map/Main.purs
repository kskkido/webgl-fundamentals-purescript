module Lib.Map.Main where

import Prelude
import Data.Map as Map
import Data.Tuple.Nested as Tuple.Nested

insert :: forall k v. Ord k => Tuple.Nested.Tuple2 k v -> Map.Map k v -> Map.Map k v
insert tx mx = Map.insert (Tuple.Nested.get1 tx) (Tuple.Nested.get2 tx) mx
