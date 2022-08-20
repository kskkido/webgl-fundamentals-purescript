module Lib.WebGL.Main where

import Prelude
import Effect as Effect
import Data.Int
import Data.Int.Bits
import Data.Either as Either
import Data.Traversable as Traversable
import Data.ArrayBuffer.Types as Data.ArrayBuffer.Types
import Control.Monad.Except.Trans as ExceptT
import Graphics.Canvas as Graphics.Canvas
import Lib.ExceptT.Main as Lib.ExceptT
import Lib.Canvas.Main as Lib.Canvas
import Lib.Canvas.Models.Dimension.Main as Lib.Canvas.Models.Dimension
import Lib.Window.Canvas.Main as Lib.Window.Canvas
import Lib.Window.WebGL.Main as Lib.Window.WebGL
import Lib.Window.WebGL.Types as Lib.Window.WebGL.Types
import Lib.Window.WebGL.Constants as Lib.Window.WebGL.Constants
import Lib.WebGL.Models.AttributeConfig.Main as Models.AttributeConfig
import Lib.WebGL.Models.Texture2dConfig.Main as Models.Texture2dConfig
import Lib.WebGL.Models.Texture2dParameters.Main as Models.Texture2dParameters
import Lib.WebGL.Models.RenderArrayConfig.Main as Models.RenderArrayConfig

createProgram :: Array Lib.Window.WebGL.Types.WebGLShader -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Lib.Window.WebGL.Types.WebGLProgram
createProgram shaders webgl = do
  program <-
    ( Lib.Window.WebGL.createProgram webgl #
      Lib.ExceptT.fromMaybeTrans "Unable to create program"
    )
  ( do
      flip Traversable.traverse_ shaders $ ExceptT.lift <<< Lib.Window.WebGL.attachShader webgl program
      ExceptT.lift $ Lib.Window.WebGL.linkProgram webgl program
      status <-
        ( Lib.Window.WebGL.getProgramParameter webgl program Lib.Window.WebGL.Constants.linkStatus #
          Lib.ExceptT.fromMaybeTrans "Unable to find status"
        )
      pure program
      `ExceptT.catchError` \e -> do
        ExceptT.lift $ Lib.Window.WebGL.deleteProgram webgl program
        ExceptT.except $ Either.Left e
  )

createVertexShader :: String -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Lib.Window.WebGL.Types.WebGLShader
createVertexShader source webgl = do
  shader <-
    ( Lib.Window.WebGL.createShader webgl Lib.Window.WebGL.Constants.vertexShader #
      Lib.ExceptT.fromMaybeTrans "Unable to initialize vertex shader"
    )
  ( do
      ExceptT.lift $ Lib.Window.WebGL.shaderSource webgl shader source
      ExceptT.lift $ Lib.Window.WebGL.compileShader webgl shader
      status <-
        ( Lib.Window.WebGL.getShaderParameter webgl shader Lib.Window.WebGL.Constants.compileStatus #
          Lib.ExceptT.fromMaybeTrans "Unable to find status"
        )
      pure shader
      `ExceptT.catchError` \e -> do
        ExceptT.lift $ Lib.Window.WebGL.deleteShader webgl shader
        ExceptT.except $ Either.Left e
  )

createFragmentShader :: String -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Lib.Window.WebGL.Types.WebGLShader
createFragmentShader source webgl = do
  shader <-
    ( Lib.Window.WebGL.createShader webgl Lib.Window.WebGL.Constants.fragmentShader #
      Lib.ExceptT.fromMaybeTrans "Unable to initialize fragment shader"
    )
  ( do
      ExceptT.lift $ Lib.Window.WebGL.shaderSource webgl shader source
      ExceptT.lift $ Lib.Window.WebGL.compileShader webgl shader
      status <-
        ( Lib.Window.WebGL.getShaderParameter webgl shader Lib.Window.WebGL.Constants.compileStatus #
          Lib.ExceptT.fromMaybeTrans "Unable to find status"
        )
      pure shader
      `ExceptT.catchError` \e -> do
        ExceptT.lift $ Lib.Window.WebGL.deleteShader webgl shader
        ExceptT.except $ Either.Left e
  )

createBuffer :: Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Lib.Window.WebGL.Types.WebGLBuffer
createBuffer webgl = do
  buffer <-
    ( Lib.Window.WebGL.createBuffer webgl #
      Lib.ExceptT.fromMaybeTrans "Unable to create buffer"
    )
  pure buffer

createTexture :: Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Lib.Window.WebGL.Types.WebGLTexture
createTexture webgl = do
  texture <-
    ( Lib.Window.WebGL.createTexture webgl #
      Lib.ExceptT.fromMaybeTrans "Unable to create texture"
    )
  pure texture

useProgram :: Lib.Window.WebGL.Types.WebGLProgram -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
useProgram program webgl = do
  ExceptT.lift $ Lib.Window.WebGL.useProgram webgl program

getUniformLocation :: String -> Lib.Window.WebGL.Types.WebGLProgram -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Lib.Window.WebGL.Types.WebGLUniformLocation
getUniformLocation name program webgl = do
  uniform <-
    ( Lib.Window.WebGL.getUniformLocation webgl program name #
      Lib.ExceptT.fromMaybeTrans "Unable to locate uniform"
    )
  pure uniform

getAttribLocation :: String -> Lib.Window.WebGL.Types.WebGLProgram -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Lib.Window.WebGL.Types.GLint
getAttribLocation name program webgl = do
  attribute <- ExceptT.lift $ Lib.Window.WebGL.getAttribLocation webgl program name
  pure attribute

getCanvas :: Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Graphics.Canvas.CanvasElement
getCanvas webgl = do
  canvas <- ExceptT.lift $ Lib.Window.WebGL.getCanvas webgl
  pure canvas

getCanvasDimension :: Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Lib.Canvas.Models.Dimension.Dimension
getCanvasDimension webgl = do
  canvas    <- getCanvas webgl
  dimension <- ExceptT.lift $ Lib.Canvas.getDimension canvas
  pure dimension

setArrayBuffer :: Lib.Window.WebGL.Types.WebGLBuffer -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
setArrayBuffer buffer webgl = do
  bindBuffer Lib.Window.WebGL.Constants.arrayBuffer buffer webgl

putArrayBuffer :: Lib.Window.WebGL.Types.BufferDataSource -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
putArrayBuffer xs webgl = do
  bufferData Lib.Window.WebGL.Constants.arrayBuffer xs Lib.Window.WebGL.Constants.staticDraw webgl

setAttributeConfig :: Models.AttributeConfig.AttributeConfig -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
setAttributeConfig config webgl = do
  enableVertexAttribArray config.index webgl
  vertexAttribPointer config.index config.size config.type config.normalize config.stride config.offset webgl

setTexture2d :: Lib.Window.WebGL.Types.WebGLTexture -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
setTexture2d texture webgl = do
  bindTexture Lib.Window.WebGL.Constants.texture2d texture webgl

setTexture2dConfigs :: Array Models.Texture2dConfig.Texture2dConfig -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
setTexture2dConfigs configs webgl = do
  flip Traversable.traverse_ configs $ \config -> setTexture2dConfig config webgl

setTexture2dConfig :: Models.Texture2dConfig.Texture2dConfig -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
setTexture2dConfig config webgl = do
  texParameteri Lib.Window.WebGL.Constants.texture2d config.pname config.param webgl

putTexture2d :: Models.Texture2dParameters.Texture2dParameters -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
putTexture2d params webgl = do
  texImage2d Lib.Window.WebGL.Constants.texture2d params.level params.internalFormat params.format params.type params.source webgl

renderArray :: Models.RenderArrayConfig.RenderArrayConfig -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
renderArray config webgl = do
  drawArrays config.mode config.first config.count webgl

resetViewport :: Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
resetViewport webgl = do
  canvas <- getCanvas webgl
  ExceptT.lift $ Lib.Window.Canvas.resize canvas
  dimension <- getCanvasDimension webgl
  viewport 0 0 (floor dimension.width) (floor dimension.height) webgl
  clearColor 0.0 0.0 0.0 0.0 webgl
  clear (Lib.Window.WebGL.Constants.colorBufferBit .|. Lib.Window.WebGL.Constants.depthBufferBit) webgl

viewport :: Lib.Window.WebGL.Types.GLint -> Lib.Window.WebGL.Types.GLint -> Lib.Window.WebGL.Types.GLsizei -> Lib.Window.WebGL.Types.GLsizei -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
viewport x y width height webgl = do
  ExceptT.lift $ Lib.Window.WebGL.viewport webgl x y width height

clearColor :: Lib.Window.WebGL.Types.GLclampf -> Lib.Window.WebGL.Types.GLclampf -> Lib.Window.WebGL.Types.GLclampf -> Lib.Window.WebGL.Types.GLclampf -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
clearColor a b c d webgl = do
  ExceptT.lift $ Lib.Window.WebGL.clearColor webgl a b c d

clear :: Lib.Window.WebGL.Types.GLbitfield -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
clear x webgl = do
  ExceptT.lift $ Lib.Window.WebGL.clear webgl x

uniform2f :: Lib.Window.WebGL.Types.GLfloat -> Lib.Window.WebGL.Types.GLfloat -> Lib.Window.WebGL.Types.WebGLUniformLocation -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
uniform2f x y location webgl = do
  ExceptT.lift $ Lib.Window.WebGL.uniform2f webgl location x y

uniform1f :: Lib.Window.WebGL.Types.GLfloat -> Lib.Window.WebGL.Types.WebGLUniformLocation -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
uniform1f x location webgl = do
  ExceptT.lift $ Lib.Window.WebGL.uniform1f webgl location x

uniform1fv :: Array Lib.Window.WebGL.Types.GLfloat -> Lib.Window.WebGL.Types.WebGLUniformLocation -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
uniform1fv xs location webgl = do
  ExceptT.lift $ Lib.Window.WebGL.uniform1fv webgl location xs

uniform3fv :: Array Lib.Window.WebGL.Types.GLfloat -> Lib.Window.WebGL.Types.WebGLUniformLocation -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
uniform3fv xs location webgl = do
  ExceptT.lift $ Lib.Window.WebGL.uniform3fv webgl location xs

uniform4fv :: Array Lib.Window.WebGL.Types.GLfloat -> Lib.Window.WebGL.Types.WebGLUniformLocation -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
uniform4fv xs location webgl = do
  ExceptT.lift $ Lib.Window.WebGL.uniform4fv webgl location xs

uniformMatrix4fv :: Lib.Window.WebGL.Types.GLboolean -> Array Lib.Window.WebGL.Types.GLfloat -> Lib.Window.WebGL.Types.WebGLUniformLocation -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
uniformMatrix4fv bx xs location webgl = do
  ExceptT.lift $ Lib.Window.WebGL.uniformMatrix4fv webgl location bx xs

uniformMatrix4fvFloatArray :: Lib.Window.WebGL.Types.GLboolean -> Lib.Window.WebGL.Types.FloatArray -> Lib.Window.WebGL.Types.WebGLUniformLocation -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
uniformMatrix4fvFloatArray bx xs location webgl = do
  ExceptT.lift $ Lib.Window.WebGL.uniformMatrix4fv_ webgl location bx xs

bindBuffer :: Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.WebGLBuffer -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
bindBuffer x buffer webgl = do
  ExceptT.lift $ Lib.Window.WebGL.bindBuffer webgl x buffer

bufferData :: Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.BufferDataSource -> Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
bufferData x xs z webgl = do
  ExceptT.lift $ Lib.Window.WebGL.bufferData webgl x xs z

bindTexture :: Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.WebGLTexture -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
bindTexture target texture webgl = do
  ExceptT.lift $ Lib.Window.WebGL.bindTexture webgl target texture

texParameteri :: Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.GLenum ->Lib.Window.WebGL.Types.GLint -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
texParameteri target pname param webgl = do
  ExceptT.lift $ Lib.Window.WebGL.texParameteri webgl target pname param

texImage2d :: Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.GLint -> Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.TexImageSource -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
texImage2d target level internalFormat format type' source webgl = do
  ExceptT.lift $ Lib.Window.WebGL.texImage2D webgl target level internalFormat format type' source

enableVertexAttribArray :: Lib.Window.WebGL.Types.GLuint -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
enableVertexAttribArray index webgl = do
  ExceptT.lift $ Lib.Window.WebGL.enableVertexAttribArray webgl index

vertexAttribPointer :: Lib.Window.WebGL.Types.GLuint -> Lib.Window.WebGL.Types.GLint -> Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.GLboolean -> Lib.Window.WebGL.Types.GLsizei -> Lib.Window.WebGL.Types.GLintptr -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
vertexAttribPointer indx size type' normalized stride offset webgl = do
  ExceptT.lift $ Lib.Window.WebGL.vertexAttribPointer webgl indx size type' normalized stride offset

drawArrays :: Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.GLint -> Lib.Window.WebGL.Types.GLsizei -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
drawArrays mode first count webgl = do
  ExceptT.lift $ Lib.Window.WebGL.drawArrays webgl mode first count

enable :: Lib.Window.WebGL.Types.GLenum -> Lib.Window.WebGL.Types.WebGLContext -> ExceptT.ExceptT String Effect.Effect Unit
enable cap webgl = do
  ExceptT.lift $ Lib.Window.WebGL.enable webgl cap
