module Pages.Shaders.Rectangle.Main where

import Prelude
import Effect as Effect
import Effect.Class as Effect.Class
import Effect.Aff as Effect.Aff
import Effect.Console as Effect.Console
import Effect.Exception as Effect.Exception
import Data.Either as Either
import Control.Monad.Except.Trans as ExceptT
import Graphics.Canvas as Graphics.Canvas
import Lib.ExceptT.Main as Lib.ExceptT
import Lib.Kernel.Main as Lib.Kernel
import Lib.Window.Canvas.Main as Lib.Window.Canvas
import Lib.Window.Image.Main as Lib.Window.Image
import Shaders.Rectangle.Main as Shaders.Rectangle

main :: Effect.Effect Unit
main = do
  Effect.Aff.launchAff_ do
    result <- ExceptT.runExceptT $ do
      ExceptT.mapExceptT Effect.Class.liftEffect do
        canvas <-
          ( Graphics.Canvas.getCanvasElementById "canvas" #
            Lib.ExceptT.fromMaybeTrans "Unable to locate canvas"
          )
        ExceptT.lift $ Effect.Console.log "Found canvas"
        Shaders.Rectangle.main
          { canvas: canvas
          }
    ( Either.either (Effect.Exception.throwException <<< Effect.Exception.error) pure result #
      Effect.Class.liftEffect
    )
  Effect.Console.log "🍝"

