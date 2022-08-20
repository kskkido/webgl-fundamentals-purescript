module Shaders.Rectangle.Main where

import Prelude
import Effect as Effect
import Effect.Random as Effect.Random
import Data.Int as Int
import Data.ArrayBuffer.Typed as Data.ArrayBuffer.Typed
import Data.List as List
import Data.Traversable as Traversable
import Effect.Console as Effect.Console
import Control.Monad.Except.Trans as ExceptT
import Lib.ExceptT.Main as Lib.ExceptT
import Lib.Window.Canvas.Main as Lib.Window.Canvas
import Lib.Window.Image.Main as Lib.Window.Image
import Lib.Window.WebGL.Main as Lib.Window.WebGL
import Lib.Window.WebGL.Constants as Lib.Window.WebGL.Constants
import Lib.Kernel.Main as Lib.Kernel
import Lib.WebGL.Main as Lib.WebGL
import Lib.Image.Main as Lib.Image
import Lib.Float32Array.Main as Lib.Float32Array
import Lib.Graphics.Models.Rectangle.Main as Lib.Graphics.Models.Rectangle
import Lib.Graphics.Models.RectangleColor.Main as Lib.Graphics.Models.RectangleColor
import Shaders.Rectangle.Models.Env.Main as Env

main :: Env.Env -> ExceptT.ExceptT String Effect.Effect Unit
main env = do
  webgl <-
    ( Lib.Window.Canvas.getWebGLContext env.canvas #
      Lib.ExceptT.fromMaybeTrans "Unable to create webgl context"
    )
  program <- do
    vertex   <- Lib.WebGL.createVertexShader vertexShaderSource webgl
    fragment <- Lib.WebGL.createFragmentShader fragmentShaderSource webgl
    Lib.WebGL.createProgram [vertex, fragment] webgl
  uniforms <- do
    resolution   <- Lib.WebGL.getUniformLocation "u_resolution" program webgl
    color        <- Lib.WebGL.getUniformLocation "u_color" program webgl
    pure $
      { resolution: resolution
      , color: color
      }
  attributes <- do
    position <- do
      location <- Lib.WebGL.getAttribLocation "a_position" program webgl
      pure $
        { index: location
        , size: 2
        , type: Lib.Window.WebGL.Constants.float
        , normalize: false
        , stride: 0
        , offset: 0
        }
    color <- do
      location <- Lib.WebGL.getAttribLocation "a_color" program webgl
      pure $
        { index: location
        , size: 2
        , type: Lib.Window.WebGL.Constants.float
        , normalize: false
        , stride: 0
        , offset: 0
        }
    pure $
      { position: position
      , color: color
      }
  buffers <- do
    position <- Lib.WebGL.createBuffer webgl
    color <- Lib.WebGL.createBuffer webgl
    pure $
      { position: position
      , color: color
      }
  dimensions <- do
    canvas <- Lib.WebGL.getCanvasDimension webgl
    pure $
      { canvas: canvas
      }
  Lib.WebGL.resetViewport webgl
  Lib.WebGL.useProgram program webgl
  Lib.WebGL.uniform2f dimensions.canvas.width dimensions.canvas.height uniforms.resolution webgl
  Lib.WebGL.setArrayBuffer buffers.color webgl
  Lib.WebGL.setAttributeConfig attributes.color webgl
  Lib.WebGL.setArrayBuffer buffers.position webgl
  Lib.WebGL.setAttributeConfig attributes.position webgl
  Lib.WebGL.setArrayBuffer buffers.position webgl
  flip Traversable.traverse_ (List.range 0 100) $ \i -> do
    shape <- ExceptT.lift do
      x <- Effect.Random.randomRange 0.0 dimensions.canvas.width
      y <- Effect.Random.randomRange 0.0 dimensions.canvas.height
      width  <- Effect.Random.randomRange 0.0 dimensions.canvas.width
      height <- Effect.Random.randomRange 0.0 dimensions.canvas.height
      array  <- pure $ Lib.Graphics.Models.Rectangle.toArray { x: x, y: y, width: width, height: height }
      ( Lib.Float32Array.toArrayBuffer <$>
        Lib.Float32Array.fromArray array
      )
    color <- ExceptT.lift do
      array <- Lib.Graphics.Models.RectangleColor.fromRandom
      ( Lib.Float32Array.toArrayBuffer <$>
        Lib.Float32Array.fromArray array
      )
    Lib.WebGL.setArrayBuffer buffers.position webgl
    Lib.WebGL.putArrayBuffer shape webgl
    Lib.WebGL.setArrayBuffer buffers.color webgl
    Lib.WebGL.putArrayBuffer color webgl
    flip Lib.WebGL.renderArray webgl
      { mode: Lib.Window.WebGL.Constants.triangles
      , first: 0
      , count: 6
      }

vertexShaderSource :: String
vertexShaderSource = """
// an attribute will receive data from a buffer
attribute vec2 a_position;
attribute vec4 a_color;
uniform vec2 u_resolution;
varying vec4 v_color;

// all shaders have a main function
void main() {
  // convert the position from pixels to 0.0 to 1.0
  vec2 zeroToOne = a_position / u_resolution;

  // convert from 0->1 to 0->2
  vec2 zeroToTwo = zeroToOne * 2.0;

  // convert from 0->2 to -1->+1 (clip space)
  vec2 clipSpace = zeroToTwo - 1.0;

  gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);;
  v_color = a_color;
}
"""

fragmentShaderSource :: String
fragmentShaderSource = """
// fragment shaders don't have a default precision so we need
// to pick one. mediump is a good default
precision mediump float;
uniform vec4 u_color;
varying vec4 v_color;

void main() {
  gl_FragColor = u_color;
  gl_FragColor = v_color;
}
"""
