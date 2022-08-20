module Shaders.Shape3d.Main where

import Prelude
import Effect as Effect
import Effect.Random as Effect.Random
import Data.Int as Int
import Data.ArrayBuffer.Typed as Data.ArrayBuffer.Typed
import Data.Array as Array
import Data.List as List
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Data.Tuple.Nested as Tuple.Nested
import Effect.Console as Effect.Console
import Control.Monad.Except.Trans as ExceptT
import Lib.ExceptT.Main as Lib.ExceptT
import Lib.Window.Canvas.Main as Lib.Window.Canvas
import Lib.Window.Image.Main as Lib.Window.Image
import Lib.Window.WebGL.Main as Lib.Window.WebGL
import Lib.Window.WebGL.Constants as Lib.Window.WebGL.Constants
import Lib.Array.Main as Lib.Array
import Lib.Kernel.Main as Lib.Kernel
import Lib.WebGL.Main as Lib.WebGL
import Lib.Image.Main as Lib.Image
import Lib.Float32Array.Main as Lib.Float32Array
import Lib.Math.Main as Lib.Math
import Lib.Matrix.Main as Lib.Matrix
import Lib.Matrix4.Main as Lib.Matrix4
import Lib.Coordinate2d.Main as Lib.Coordinate2d
import Lib.Graphics.Models.AlphabetF3d.Main as Lib.Graphics.Models.AlphabetF3d
import Lib.Graphics.Models.AlphabetF3dColor.Main as Lib.Graphics.Models.AlphabetF3dColor
import Lib.Graphics.Models.Rectangle.Main as Lib.Graphics.Models.Rectangle
import Lib.Graphics.Models.RectangleColor.Main as Lib.Graphics.Models.RectangleColor
import Shaders.Shape3d.Models.DrawConfig.Main as DrawConfig
import Shaders.Shape3d.Models.Context.Main as Context
import Shaders.Shape3d.Models.Env.Main as Env

main :: DrawConfig.DrawConfig -> Context.Context -> ExceptT.ExceptT String Effect.Effect Unit
main config context = do
  dimensions <- do
    canvas <- Lib.WebGL.getCanvasDimension context.webgl
    pure $
      { canvas:
          { x: canvas.width
          , y: canvas.height
          , z: 400.0
          }
      }
  matrices <- do
    pure
      ( Array.replicate 10
        ( [ Lib.Matrix4.translate $ Tuple.Nested.tuple3
              (Lib.Math.clamp config.position.x 0.0 dimensions.canvas.x)
              (Lib.Math.clamp config.position.y 0.0 dimensions.canvas.y)
              (Lib.Math.clamp config.position.z 0.0 dimensions.canvas.z)
          , Lib.Matrix4.rotateX $ Lib.Coordinate2d.fromDegree config.rotation.x
          , Lib.Matrix4.rotateY $ Lib.Coordinate2d.fromDegree config.rotation.y
          , Lib.Matrix4.rotateZ $ Lib.Coordinate2d.fromDegree config.rotation.z
          , Lib.Matrix4.scale $ Tuple.Nested.tuple3
              (config.scale.x)
              (config.scale.y)
              (config.scale.z)
          ] #
          Foldable.foldl Lib.Matrix.multiply Lib.Matrix4.unit
        ) #
        Lib.Array.revolving
          Lib.Matrix.multiply
          ( Lib.Matrix4.projection $ Tuple.Nested.tuple3
              (dimensions.canvas.x)
              (dimensions.canvas.y)
              (dimensions.canvas.z)
          )
      )
  Lib.WebGL.enable Lib.Window.WebGL.Constants.cullFace context.webgl
  Lib.WebGL.enable Lib.Window.WebGL.Constants.depthTest context.webgl
  Lib.WebGL.resetViewport context.webgl
  Lib.WebGL.useProgram context.program context.webgl
  Lib.WebGL.setArrayBuffer context.buffers.color context.webgl
  Lib.WebGL.setAttributeConfig context.attributes.color context.webgl
  Lib.WebGL.setArrayBuffer context.buffers.position context.webgl
  Lib.WebGL.setAttributeConfig context.attributes.position context.webgl
  flip Traversable.traverse_ matrices $ \matrix -> do
    let mx = matrix # Lib.Array.transpose # Array.concat
    Lib.WebGL.uniformMatrix4fv false mx context.uniforms.matrix context.webgl
    flip Lib.WebGL.renderArray context.webgl
      { mode: Lib.Window.WebGL.Constants.triangles
      , first: 0
      , count: 16 * 6
      }

