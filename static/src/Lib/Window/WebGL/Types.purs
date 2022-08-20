module Lib.Window.WebGL.Types where

import Data.ArrayBuffer.Types
import Lib.Effect.Main

type DOMString        = String
type BufferDataSource = ArrayBuffer
type FloatArray       = Float32Array
type GLbitfield       = Int
type GLboolean        = Boolean
type GLbyte           = Int
type GLclampf         = Number
type GLenum           = Int
type GLfloat          = Number
type GLint            = Int
type GLintptr         = Int
type GLshort          = Int
type GLsizei          = Int
type GLsizeiptr       = Int
type GLubyte          = Int
type GLuint           = Int
type GLushort         = Int

foreign import data Canvas :: Effect
foreign import data ArrayBufferView :: Type
foreign import data TexImageSource :: Type
foreign import data WebGLActiveInfo :: Type
foreign import data WebGLBuffer :: Type
foreign import data WebGLContext :: Type
foreign import data WebGLFramebuffer :: Type
foreign import data WebGLProgram :: Type
foreign import data WebGLRenderbuffer :: Type
foreign import data WebGLShader :: Type
foreign import data WebGLShaderPrecisionFormat :: Type
foreign import data WebGLTexture :: Type
foreign import data WebGLUniformLocation :: Type

type WebGLContextAttributes =
  { alpha                           :: Boolean
  , depth                           :: Boolean
  , stencil                         :: Boolean
  , antialias                       :: Boolean
  , premultipliedAlpha              :: Boolean
  , preserveDrawingBuffer           :: Boolean
  , preferLowPowerToHighPerformance :: Boolean
  , failIfMajorPerformanceCaveat    :: Boolean
  }

