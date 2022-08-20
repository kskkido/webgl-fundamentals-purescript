module Pages.Home.Main where

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
import Shaders.Texture2d.Main as Shaders.Texture2d

main :: Effect.Effect Unit
main = do
  Effect.Aff.launchAff_ do
    result <- ExceptT.runExceptT $ do
      image <- ExceptT.ExceptT $ Lib.Window.Image.fromUrl "./assets/images/kskkido_1.jpg"
      ExceptT.mapExceptT Effect.Class.liftEffect do
        canvas <-
          ( Graphics.Canvas.getCanvasElementById "canvas" #
            Lib.ExceptT.fromMaybeTrans "Unable to locate canvas"
          )
        ExceptT.lift $ Effect.Console.log "Found canvas"
        Shaders.Texture2d.main
          { canvas: canvas
          , image: image
          , kernel: Lib.Kernel.sharpness1
          }
    ( Either.either (Effect.Exception.throwException <<< Effect.Exception.error) pure result #
      Effect.Class.liftEffect
    )
  Effect.Console.log "🍝"
