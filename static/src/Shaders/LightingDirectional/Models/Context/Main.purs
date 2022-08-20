module Shaders.LightingDirectional.Models.Context.Main where

import Prelude
import Effect as Effect
import Effect.Random as Effect.Random
import Effect.Console as Effect.Console
import Control.Monad.Except.Trans as ExceptT
import Data.Int as Int
import Data.ArrayBuffer.Typed as Data.ArrayBuffer.Typed
import Data.Array as Array
import Data.List as List
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Data.Tuple.Nested as Tuple.Nested
import Lib.Array.Main as Lib.Array
import Lib.ExceptT.Main as Lib.ExceptT
import Lib.Float32Array.Main as Lib.Float32Array
import Lib.Uint8Array.Main as Lib.Uint8Array
import Lib.Image.Main as Lib.Image
import Lib.Math.Main as Lib.Math
import Lib.Matrix.Main as Lib.Matrix
import Lib.Matrix4.Main as Lib.Matrix4
import Lib.Coordinate2d.Main as Lib.Coordinate2d
import Lib.Graphics.Models.AlphabetF3d.Main as Lib.Graphics.Models.AlphabetF3d
import Lib.Graphics.Models.AlphabetF3dColor.Main as Lib.Graphics.Models.AlphabetF3dColor
import Lib.Graphics.Models.AlphabetF3dNormal.Main as Lib.Graphics.Models.AlphabetF3dNormal
import Lib.Graphics.Models.Rectangle.Main as Lib.Graphics.Models.Rectangle
import Lib.Graphics.Models.RectangleColor.Main as Lib.Graphics.Models.RectangleColor
import Lib.Window.Canvas.Main as Lib.Window.Canvas
import Lib.Window.Image.Main as Lib.Window.Image
import Lib.Window.WebGL.Main as Lib.Window.WebGL
import Lib.Window.WebGL.Constants as Lib.Window.WebGL.Constants
import Lib.Window.WebGL.Types as Lib.Window.WebGL.Types
import Lib.WebGL.Main as Lib.WebGL
import Lib.WebGL.Models.AttributeConfig.Main as Lib.WebGL.Models.AttributeConfig
import Shaders.LightingDirectional.Models.Env.Main as Env

type Context =
  { webgl :: Lib.Window.WebGL.Types.WebGLContext
  , program :: Lib.Window.WebGL.Types.WebGLProgram
  , uniforms ::
      { matrix :: Lib.Window.WebGL.Types.WebGLUniformLocation
      , camera :: Lib.Window.WebGL.Types.WebGLUniformLocation
      , reverseLightDirection :: Lib.Window.WebGL.Types.WebGLUniformLocation
      }
  , attributes ::
      { position :: Lib.WebGL.Models.AttributeConfig.AttributeConfig
      , color :: Lib.WebGL.Models.AttributeConfig.AttributeConfig
      , normal :: Lib.WebGL.Models.AttributeConfig.AttributeConfig
      }
  , buffers ::
      { position :: Lib.Window.WebGL.Types.WebGLBuffer
      , color :: Lib.Window.WebGL.Types.WebGLBuffer
      , normal :: Lib.Window.WebGL.Types.WebGLBuffer
      }
  }

fromEnv :: Env.Env -> ExceptT.ExceptT String Effect.Effect Context
fromEnv env = do
  webgl <-
    ( Lib.Window.Canvas.getWebGLContext env.canvas #
      Lib.ExceptT.fromMaybeTrans "Unable to create webgl context"
    )
  program <- do
    vertex   <- Lib.WebGL.createVertexShader vertexShaderSource webgl
    fragment <- Lib.WebGL.createFragmentShader fragmentShaderSource webgl
    Lib.WebGL.createProgram [vertex, fragment] webgl
  uniforms <- do
    matrix <- Lib.WebGL.getUniformLocation "u_matrix" program webgl
    camera <- Lib.WebGL.getUniformLocation "u_camera" program webgl
    reverseLightDirection <- Lib.WebGL.getUniformLocation "u_reverseLightDirection" program webgl
    pure $
      { matrix: matrix
      , camera: camera
      , reverseLightDirection: reverseLightDirection
      }
  attributes <- do
    position <- do
      location <- Lib.WebGL.getAttribLocation "a_position" program webgl
      pure $
        { index: location
        , size: 3
        , type: Lib.Window.WebGL.Constants.float
        , normalize: false
        , stride: 0
        , offset: 0
        }
    color <- do
      location <- Lib.WebGL.getAttribLocation "a_color" program webgl
      pure $
        { index: location
        , size: 3
        , type: Lib.Window.WebGL.Constants.unsignedByte
        , normalize: true
        , stride: 0
        , offset: 0
        }
    normal <- do
      location <- Lib.WebGL.getAttribLocation "a_normal" program webgl
      pure $
        { index: location
        , size: 3
        , type: Lib.Window.WebGL.Constants.float
        , normalize: false
        , stride: 0
        , offset: 0
        }
    pure $
      { position: position
      , color: color
      , normal: normal
      }
  buffers <- do
    position <- do
      buffer <- Lib.WebGL.createBuffer webgl
      stream <- ExceptT.lift $
        ( Lib.Float32Array.toArrayBuffer <$>
          Lib.Float32Array.fromArray Lib.Graphics.Models.AlphabetF3d.array
        )
      Lib.WebGL.setArrayBuffer buffer webgl
      Lib.WebGL.putArrayBuffer stream webgl
      pure buffer
    color <- do
      buffer <- Lib.WebGL.createBuffer webgl
      stream <- ExceptT.lift $
        ( Lib.Uint8Array.toArrayBuffer <$>
          Lib.Uint8Array.fromArray Lib.Graphics.Models.AlphabetF3dColor.array
        )
      Lib.WebGL.setArrayBuffer buffer webgl
      Lib.WebGL.putArrayBuffer stream webgl
      pure buffer
    normal <- do
      buffer <- Lib.WebGL.createBuffer webgl
      stream <- ExceptT.lift $
        ( Lib.Float32Array.toArrayBuffer <$>
          Lib.Float32Array.fromArray Lib.Graphics.Models.AlphabetF3dNormal.array
        )
      Lib.WebGL.setArrayBuffer buffer webgl
      Lib.WebGL.putArrayBuffer stream webgl
      pure buffer
    pure $
      { position: position
      , color: color
      , normal: normal
      }
  pure $
    { webgl: webgl
    , program: program
    , uniforms: uniforms
    , attributes: attributes
    , buffers: buffers
    }

vertexShaderSource :: String
vertexShaderSource = """
attribute vec4 a_position;
attribute vec4 a_color;
attribute vec3 a_normal;
uniform mat4 u_matrix;
uniform mat4 u_camera;
varying vec4 v_color;
varying vec3 v_normal;

void main() {
  gl_Position = u_matrix * a_position;
  v_color = a_color;
  v_normal = mat3(u_camera) * a_normal;
}
"""

fragmentShaderSource :: String
fragmentShaderSource = """
precision mediump float;

varying vec4 v_color;
varying vec3 v_normal;

uniform vec3 u_reverseLightDirection;

void main() {
  gl_FragColor = v_color;
  gl_FragColor.rgb *= dot(normalize(v_normal), u_reverseLightDirection);
}
"""


