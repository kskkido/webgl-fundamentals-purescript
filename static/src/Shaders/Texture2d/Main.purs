module Shaders.Texture2d.Main where

import Prelude
import Effect as Effect
import Data.Int as Int
import Data.ArrayBuffer.Typed as Data.ArrayBuffer.Typed
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
import Shaders.Texture2d.Models.Env.Main as Env

main :: Env.Env -> ExceptT.ExceptT String Effect.Effect Unit
main env = do
  webgl <-
    ( Lib.Window.Canvas.getWebGLContext env.canvas #
      Lib.ExceptT.fromMaybeTrans "Unable to create webgl context"
    )
  shaders <- do
    vertex   <- Lib.WebGL.createVertexShader vertexShaderSource webgl
    fragment <- Lib.WebGL.createFragmentShader fragmentShaderSource webgl
    pure $
      { vertex: vertex
      , fragment: fragment
      }
  program <- Lib.WebGL.createProgram [shaders.vertex, shaders.fragment] webgl
  uniforms <- do
    textureSize  <- Lib.WebGL.getUniformLocation "u_textureSize" program webgl
    resolution   <- Lib.WebGL.getUniformLocation "u_resolution" program webgl
    kernel       <- Lib.WebGL.getUniformLocation "u_kernel" program webgl
    kernelWeight <- Lib.WebGL.getUniformLocation "u_kernelWeight" program webgl
    pure $
      { textureSize: textureSize
      , resolution: resolution
      , kernel: kernel
      , kernelWeight: kernelWeight
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
    texture <- do
      location <- Lib.WebGL.getAttribLocation "a_textureCoordinate" program webgl
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
      , texture: texture
      }
  buffers <- do
    position <- Lib.WebGL.createBuffer webgl
    texture <- Lib.WebGL.createBuffer webgl
    pure $
      { position: position
      , texture: texture
      }
  textures <- do
    x <- Lib.WebGL.createTexture webgl
    pure $
      { x: x
      }
  dimensions <- do
    image <- ExceptT.lift $ Lib.Image.getDimension env.image
    canvas <- Lib.WebGL.getCanvasDimension webgl
    pure $
      { image: image
      , canvas: canvas
      }
  Lib.WebGL.resetViewport webgl
  Lib.WebGL.useProgram program webgl
  Lib.WebGL.uniform2f dimensions.image.width dimensions.image.height uniforms.textureSize webgl
  Lib.WebGL.uniform2f dimensions.canvas.width dimensions.canvas.height uniforms.resolution webgl
  Lib.WebGL.uniform1fv env.kernel uniforms.kernel webgl
  Lib.WebGL.uniform1f (Lib.Kernel.toWeight env.kernel) uniforms.kernelWeight webgl
  Lib.WebGL.setArrayBuffer buffers.position webgl
  position <- ExceptT.lift do
    let model = { x: 0.0, y: 0.0, width: 500.0, height: 500.0 }
    ( Lib.Float32Array.toArrayBuffer <$>
      Lib.Float32Array.fromArray (Lib.Graphics.Models.Rectangle.toArray model)
    )
  Lib.WebGL.putArrayBuffer position webgl
  Lib.WebGL.setAttributeConfig attributes.position webgl
  Lib.WebGL.setArrayBuffer buffers.texture webgl
  texture <- ExceptT.lift do
    let model = [0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0]
    ( Lib.Float32Array.toArrayBuffer <$>
      Lib.Float32Array.fromArray model
    )
  Lib.WebGL.putArrayBuffer texture webgl
  Lib.WebGL.setAttributeConfig attributes.texture webgl
  Lib.WebGL.setTexture2d textures.x webgl
  flip Lib.WebGL.setTexture2dConfigs webgl
    [ { pname: Lib.Window.WebGL.Constants.textureWrapS, param: Lib.Window.WebGL.Constants.clampToEdge }
    , { pname: Lib.Window.WebGL.Constants.textureWrapT, param: Lib.Window.WebGL.Constants.clampToEdge }
    , { pname: Lib.Window.WebGL.Constants.textureMinFilter, param: Lib.Window.WebGL.Constants.nearest }
    , { pname: Lib.Window.WebGL.Constants.textureMagFilter, param: Lib.Window.WebGL.Constants.nearest }
    ]
  flip Lib.WebGL.putTexture2d webgl
    { level: 0
    , internalFormat: Lib.Window.WebGL.Constants.rgba
    , format: Lib.Window.WebGL.Constants.rgba
    , type: Lib.Window.WebGL.Constants.unsignedByte
    , source: Lib.Window.Image.toTexImageSource env.image
    }
  flip Lib.WebGL.renderArray webgl
    { mode: Lib.Window.WebGL.Constants.triangles
    , first: 0
    , count: 6
    }

vertexShaderSource :: String
vertexShaderSource = """
attribute vec2 a_position;
attribute vec2 a_textureCoordinate;

uniform vec2 u_resolution;

varying vec2 v_textureCoordinate;

void main() {
  // convert the rectangle from pixels to 0.0 to 1.0
  vec2 zeroToOne = a_position / u_resolution;

  // convert from 0->1 to 0->2
  vec2 zeroToTwo = zeroToOne * 2.0;

  // convert from 0->2 to -1->+1 (clipspace)
  vec2 clipSpace = zeroToTwo - 1.0;

  gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);

  // pass the texCoord to the fragment shader
  // The GPU will interpolate this value between points.
  v_textureCoordinate = a_textureCoordinate;
}
"""

fragmentShaderSource :: String
fragmentShaderSource = """
precision mediump float;

// our texture
uniform sampler2D u_image;
uniform vec2 u_textureSize;
uniform float u_kernel[9];
uniform float u_kernelWeight;

// the texCoords passed in from the vertex shader.
varying vec2 v_textureCoordinate;

void main() {
  vec2 onePixel = vec2(1.0, 1.0) / u_textureSize;
  vec4 colorSum =
      texture2D(u_image, v_textureCoordinate + onePixel * vec2(-1, -1)) * u_kernel[0] +
      texture2D(u_image, v_textureCoordinate + onePixel * vec2( 0, -1)) * u_kernel[1] +
      texture2D(u_image, v_textureCoordinate + onePixel * vec2( 1, -1)) * u_kernel[2] +
      texture2D(u_image, v_textureCoordinate + onePixel * vec2(-1,  0)) * u_kernel[3] +
      texture2D(u_image, v_textureCoordinate + onePixel * vec2( 0,  0)) * u_kernel[4] +
      texture2D(u_image, v_textureCoordinate + onePixel * vec2( 1,  0)) * u_kernel[5] +
      texture2D(u_image, v_textureCoordinate + onePixel * vec2(-1,  1)) * u_kernel[6] +
      texture2D(u_image, v_textureCoordinate + onePixel * vec2( 0,  1)) * u_kernel[7] +
      texture2D(u_image, v_textureCoordinate + onePixel * vec2( 1,  1)) * u_kernel[8] ;
  gl_FragColor = vec4((colorSum / u_kernelWeight).bgr, 1);
}
"""
