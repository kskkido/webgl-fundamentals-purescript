module Lib.ExceptT.Main where

import Prelude
import Data.Maybe as Maybe
import Data.Either as Either
import Control.Monad.Except.Trans as ExceptT
import Control.Monad.Maybe.Trans as MaybeT
import Effect as Effect

fromMaybeTrans :: forall a m b. Monad m => a -> m (Maybe.Maybe b) -> ExceptT.ExceptT a m b
fromMaybeTrans l mmx = ExceptT.ExceptT $ do
  let el = Either.Left l
  Maybe.maybe el Either.Right <$> mmx

fromMaybeT :: forall a m b. Monad m => a -> MaybeT.MaybeT m b -> ExceptT.ExceptT a m b
fromMaybeT l mmx = fromMaybeTrans l $ MaybeT.runMaybeT mmx

