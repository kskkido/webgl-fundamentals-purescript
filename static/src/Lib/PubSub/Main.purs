module Lib.PubSub.Main where

import Prelude
import Effect as Effect
import Effect.Ref as Effect.Ref
import Effect.Unsafe as Effect.Unsafe
import Effect.Console as Effect.Console
import Data.Maybe as Maybe
import Data.Traversable as Traversable
import Data.Tuple.Nested as Tuple.Nested
import Control.Applicative as Applicative
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as ST.Ref
import Control.Monad.Cont.Trans as ContT
import Web.HTML.Window as Web.HTML.Window
import Web.Event.Event as Web.Event.Event
import Web.Event.EventTarget as Web.Event.EventTarget
import Web.Event.Internal.Types as Web.Event.Internal.Types
import Lib.Window.Main as Lib.Window

-- (a -> Effect.Effect ()) -> Effect.Effect ()
type PubSub a = ContT.ContT Unit Effect.Effect a

from :: forall a. a -> PubSub a
from x = ContT.ContT $ \ca -> ca x

fromRaf :: Web.HTML.Window.Window -> PubSub Number
fromRaf w = ContT.ContT $ \cn -> void $ do
  let iter n = void $ do
        cn n
        Lib.Window.requestAnimationFrame iter w
  Lib.Window.requestAnimationFrame iter w

fromEvent :: Web.Event.Event.EventType -> Web.Event.EventTarget.EventTarget -> PubSub Web.Event.Internal.Types.Event
fromEvent eventType target = ContT.ContT $ \cb -> do
  listener <- Web.Event.EventTarget.eventListener cb
  Web.Event.EventTarget.addEventListener eventType listener false target

fold :: forall a b. (a -> b -> b) -> b -> PubSub a -> PubSub b
fold step seed mx =
  let ref = Effect.Unsafe.unsafePerformEffect $ Effect.Ref.new seed
  in merge
    [ bind_ mx $ \x -> ContT.ContT $ \cb -> do
        acc <- Effect.Ref.modify (step x) ref
        cb acc
    , pure seed
    ]

merge :: forall a. Array (PubSub a) -> PubSub a
merge mxs = ContT.ContT $ \ca ->
  ( flip Traversable.traverse_ mxs $ \mx ->
    ( ContT.runContT mx ca
    )
  )

concat :: forall a b. PubSub a -> PubSub b -> PubSub (Tuple.Nested.Tuple2 a b)
concat mx my = ContT.ContT $ \cb -> do
  ry <- Effect.Ref.new Maybe.Nothing
  ContT.runContT my $ \y -> do
    Effect.Ref.write (pure y) ry
  ContT.runContT mx $ \x -> do
    my <- Effect.Ref.read ry
    Maybe.maybe (pure unit) (cb <<< Tuple.Nested.tuple2 x) my

on :: forall a b. PubSub a -> PubSub b -> PubSub b
on mx my = Tuple.Nested.get2 <$> concat mx my

filter :: forall a. (a -> Boolean) -> PubSub a -> PubSub a
filter fn mx = ContT.ContT $ \ca ->
  ( ContT.runContT mx $ \a -> do
      Applicative.when (fn a) (ca a)
  )

filterMap :: forall a b. (a -> Maybe.Maybe b) -> PubSub a -> PubSub b
filterMap fn mx = ContT.ContT $ \cb ->
  ( ContT.runContT (fn <$> mx) $ Maybe.maybe (pure unit) cb
  )

delta :: PubSub Number -> PubSub Number
delta mx =
  ( fold
    ( \curr tx ->
      ( let prev = Tuple.Nested.get1 tx
        in Tuple.Nested.tuple2 curr (curr - prev)
      )
    )
    ( Tuple.Nested.tuple2 0.0 0.0
    )
    ( mx
    )
  ) #
  map Tuple.Nested.get2

bindIO :: forall a b. (a -> Effect.Effect b) -> PubSub a -> PubSub b
bindIO fn mx = joinIO $ fn <$> mx

joinIO :: forall a. PubSub (Effect.Effect a) -> PubSub a
joinIO mmx = ContT.ContT $ \ca ->
  ( ContT.runContT mmx $ \ma -> ma >>= ca
  )

tapIO :: forall a. (a -> Effect.Effect Unit) -> PubSub a -> PubSub a
tapIO fn mx = ContT.ContT $ \ca ->
  ( ContT.runContT mx $ \a -> do
      fn a
      ca a
  )


-- practice
bind_ :: forall a b . PubSub a -> (a -> PubSub b) -> PubSub b
bind_ mx fn = ContT.ContT $ \cb ->
  ( ContT.runContT mx $ \a ->
    ( ContT.runContT (fn a) cb
    )
  )

join_ :: forall a. PubSub (PubSub a) -> PubSub a
join_ mmx = ContT.ContT $ \ca ->
  ( ContT.runContT mmx $ \mx ->
    ( ContT.runContT mx ca
    )
  )

map_ :: forall a b. (a -> b) -> PubSub a -> PubSub b
map_ fn mx = ContT.ContT $ \cb ->
  ( ContT.runContT mx (cb <<< fn)
  )

