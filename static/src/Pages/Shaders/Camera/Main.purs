module Pages.Shaders.Camera.Main where

import Prelude
import Effect as Effect
import Effect.Class as Effect.Class
import Effect.Aff as Effect.Aff
import Effect.Console as Effect.Console
import Effect.Exception as Effect.Exception
import Data.Number as Number
import Data.Map as Map
import Data.Either as Either
import Data.Maybe as Maybe
import Data.Tuple.Nested as Tuple.Nested
import Control.Monad.Cont.Trans as ContT
import Control.Monad.Except.Trans as ExceptT
import Control.Monad.Maybe.Trans as MaybeT
import Web.Event.Event as Web.Event.Event
import Web.DOM.Element as Web.DOM.Element
import Web.DOM.DOMTokenList as Web.DOM.DOMTokenList
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.HTMLInputElement as Web.HTML.HTMLInputElement
import Graphics.Canvas as Graphics.Canvas
import Lib.Map.Main as Lib.Map
import Lib.PubSub.Main as Lib.PubSub
import Lib.Canvas.Main as Lib.Canvas
import Lib.ExceptT.Main as Lib.ExceptT
import Lib.Kernel.Main as Lib.Kernel
import Lib.Element.Main as Lib.Element
import Lib.HtmlElement.Main as Lib.HtmlElement
import Lib.Window.Canvas.Main as Lib.Window.Canvas
import Lib.Window.Image.Main as Lib.Window.Image
import Shaders.Camera.Models.DrawConfig.Main as Shaders.Camera.Models.DrawConfig
import Shaders.Camera.Models.Context.Main as Shaders.Camera.Models.Context
import Shaders.Camera.Main as Shaders.Camera

main :: Effect.Effect Unit
main = do
  window <- Web.HTML.window
  document <- Web.HTML.Window.document window
  Effect.Aff.launchAff_ do
    result <- ExceptT.runExceptT $ do
      image <- ExceptT.ExceptT $ Lib.Window.Image.fromUrl "/assets/images/kskkido_1.jpg"
      ExceptT.mapExceptT Effect.Class.liftEffect do
        body <-
          ( Web.HTML.HTMLDocument.body document #
            Lib.ExceptT.fromMaybeTrans "Unable to locate body"
          )
        canvas <-
          ( Graphics.Canvas.getCanvasElementById "canvas" #
            Lib.ExceptT.fromMaybeTrans "Unable to locate canvas"
          )
        dimension <- ExceptT.lift $ Lib.Canvas.getDimension canvas
        ExceptT.lift $ Effect.Console.log "Found canvas"
        context <- Shaders.Camera.Models.Context.fromEnv
          { canvas: canvas
          }
        controller <- ExceptT.lift $ do
          angle <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show $ -360) input
            Web.DOM.Element.setAttribute "max" (show 360) input
            Web.DOM.Element.setAttribute "value" (show 0) input
            Web.DOM.Element.setAttribute "step" (show 0.01) input
            Lib.Element.setTextContent "angle: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          radius <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show $ 0) input
            Web.DOM.Element.setAttribute "max" (show 400) input
            Web.DOM.Element.setAttribute "value" (show 200) input
            Web.DOM.Element.setAttribute "step" (show 0.1) input
            Lib.Element.setTextContent "radius: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          count <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show 1) input
            Web.DOM.Element.setAttribute "max" (show 150) input
            Web.DOM.Element.setAttribute "value" (show 1) input
            Web.DOM.Element.setAttribute "step" (show 1) input
            Lib.Element.setTextContent "count: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          fieldOfView <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show 0) input
            Web.DOM.Element.setAttribute "max" (show 180) input
            Web.DOM.Element.setAttribute "value" (show 60) input
            Web.DOM.Element.setAttribute "step" (show 0.1) input
            Lib.Element.setTextContent "fieldOfView: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          x <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show (-dimension.width)) input
            Web.DOM.Element.setAttribute "max" (show dimension.width) input
            Web.DOM.Element.setAttribute "value" (show 0) input
            Lib.Element.setTextContent "x: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          y <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show (-dimension.height)) input
            Web.DOM.Element.setAttribute "max" (show dimension.height) input
            Web.DOM.Element.setAttribute "value" (show 0) input
            Lib.Element.setTextContent "y: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          z <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show $ -400.0) input
            Web.DOM.Element.setAttribute "max" (show 400.0) input
            Web.DOM.Element.setAttribute "value" (show $ -400.0) input
            Lib.Element.setTextContent "z: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          scaleX <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show $ -2) input
            Web.DOM.Element.setAttribute "max" (show 2) input
            Web.DOM.Element.setAttribute "value" (show 1) input
            Web.DOM.Element.setAttribute "step" (show 0.01) input
            Lib.Element.setTextContent "scaleX: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          scaleY <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show $ -2) input
            Web.DOM.Element.setAttribute "max" (show 2) input
            Web.DOM.Element.setAttribute "value" (show 1) input
            Web.DOM.Element.setAttribute "step" (show 0.01) input
            Lib.Element.setTextContent "scaleY: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          scaleZ <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show $ -2) input
            Web.DOM.Element.setAttribute "max" (show 2) input
            Web.DOM.Element.setAttribute "value" (show 1) input
            Web.DOM.Element.setAttribute "step" (show 0.01) input
            Lib.Element.setTextContent "scaleZ: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          rotationX <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show $ 0) input
            Web.DOM.Element.setAttribute "max" (show 360) input
            Web.DOM.Element.setAttribute "value" (show 1) input
            Lib.Element.setTextContent "rotationX: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          rotationY <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show 0) input
            Web.DOM.Element.setAttribute "max" (show 360) input
            Web.DOM.Element.setAttribute "value" (show 0) input
            Lib.Element.setTextContent "rotationY: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          rotationZ <- do
            field <- Lib.Element.fromTag "div" window
            fieldLabel <- Lib.Element.fromTag "div" window
            input <- Lib.Element.fromTag "input" window
            value <- Lib.Element.fromTag "span" window
            label <- Lib.Element.fromTag "span" window
            Lib.Element.addClass "field" field
            Lib.Element.append (Web.DOM.Element.toNode <$> [fieldLabel, input]) field
            Lib.Element.addClass "field-label" fieldLabel
            Lib.Element.append (Web.DOM.Element.toNode <$> [label, value]) fieldLabel
            Web.DOM.Element.setAttribute "type" "range" input
            Web.DOM.Element.setAttribute "min" (show 0) input
            Web.DOM.Element.setAttribute "max" (show 360) input
            Web.DOM.Element.setAttribute "value" (show 0.0) input
            Lib.Element.setTextContent "rotationZ: " label
            pure $
              { field: field
              , input: input
              , value: value
              }
          container <- Lib.Element.fromTag "div" window
          Lib.Element.addClass "gl-controller" container
          Lib.Element.append (Web.DOM.Element.toNode <$> [container]) (Web.HTML.HTMLElement.toElement body)
          flip Lib.Element.append container $ Web.DOM.Element.toNode <$>
            [ angle.field
            , radius.field
            , count.field
            , fieldOfView.field
            , x.field
            , y.field
            , z.field
            , scaleX.field
            , scaleY.field
            , scaleZ.field
            , rotationX.field
            , rotationY.field
            , rotationZ.field
            ]
          pure $
            { container: container
            , angle: angle
            , radius: radius
            , count: count
            , fieldOfView: fieldOfView
            , x: x
            , y: y
            , z: z
            , scaleX: scaleX
            , scaleY: scaleY
            , scaleZ: scaleZ
            , rotationX: rotationX
            , rotationY: rotationY
            , rotationZ: rotationZ
            }
        ExceptT.lift $ do
          subscribe <- pure $ ContT.runContT
            ( Lib.PubSub.merge
              [ ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.angle.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "angle")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.radius.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "radius")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.count.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "count")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.fieldOfView.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "fieldOfView")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.x.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "position.x")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.y.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "position.y")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.z.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "position.z")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.scaleX.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "scale.x")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.scaleY.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "scale.y")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.scaleZ.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "scale.z")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.rotationX.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "rotation.x")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.rotationY.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "rotation.y")
                )
              , ( Lib.PubSub.fromEvent (Web.Event.Event.EventType "input") (Web.DOM.Element.toEventTarget controller.rotationZ.input) #
                  Lib.PubSub.filterMap Web.Event.Event.target #
                  Lib.PubSub.filterMap Web.HTML.HTMLInputElement.fromEventTarget #
                  Lib.PubSub.bindIO Web.HTML.HTMLInputElement.value #
                  Lib.PubSub.filterMap Number.fromString #
                  map (Tuple.Nested.tuple2 "rotation.z")
                )
              ] #
              Lib.PubSub.fold Lib.Map.insert Map.empty #
              map Shaders.Camera.Models.DrawConfig.fromMap
            )
          subscribe $ \config -> do
            Lib.Element.setTextContent (show config.angle) controller.angle.value
            Lib.Element.setTextContent (show config.radius) controller.radius.value
            Lib.Element.setTextContent (show config.count) controller.count.value
            Lib.Element.setTextContent (show config.fieldOfView) controller.fieldOfView.value
            Lib.Element.setTextContent (show config.position.x) controller.x.value
            Lib.Element.setTextContent (show config.position.y) controller.y.value
            Lib.Element.setTextContent (show config.position.z) controller.z.value
            Lib.Element.setTextContent (show config.scale.x) controller.scaleX.value
            Lib.Element.setTextContent (show config.scale.y) controller.scaleY.value
            Lib.Element.setTextContent (show config.scale.z) controller.scaleZ.value
            Lib.Element.setTextContent (show config.rotation.x) controller.rotationX.value
            Lib.Element.setTextContent (show config.rotation.y) controller.rotationY.value
            Lib.Element.setTextContent (show config.rotation.z) controller.rotationZ.value
            result <- ExceptT.runExceptT $ Shaders.Camera.main config context
            ( Either.either (Effect.Exception.throwException <<< Effect.Exception.error) pure result #
              Effect.Class.liftEffect
            )
    ( Either.either (Effect.Exception.throwException <<< Effect.Exception.error) pure result #
      Effect.Class.liftEffect
    )
  Effect.Console.log "🍝"

