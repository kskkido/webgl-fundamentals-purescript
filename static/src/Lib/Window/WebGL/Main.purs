module Lib.Window.WebGL.Main where

import Prelude
import Effect as Effect
import Data.Function.Uncurried
import Data.Maybe
import Data.ArrayBuffer.Types
import Graphics.Canvas as Graphics.Canvas
import Lib.Maybe.Main as Lib.Maybe
import Lib.Window.WebGL.Utils
import Lib.Window.WebGL.Types
import Lib.Window.WebGL.Constants

foreign import activeTextureImpl :: Fn2 WebGLContext GLenum (Effect.Effect Unit)

activeTexture :: WebGLContext -> GLenum -> Effect.Effect Unit
activeTexture webgl texture = runFn2 activeTextureImpl webgl texture

foreign import attachShaderImpl :: Fn3 WebGLContext WebGLProgram WebGLShader (Effect.Effect Unit)

attachShader :: WebGLContext -> WebGLProgram -> WebGLShader -> Effect.Effect Unit
attachShader webgl program shader = runFn3 attachShaderImpl webgl program shader

foreign import bindAttribLocationImpl :: Fn4 WebGLContext WebGLProgram GLuint DOMString (Effect.Effect Unit)

bindAttribLocation :: WebGLContext -> WebGLProgram -> GLuint -> DOMString -> Effect.Effect Unit
bindAttribLocation webgl program index name = runFn4 bindAttribLocationImpl webgl program index name

foreign import bindBufferImpl :: Fn3 WebGLContext GLenum WebGLBuffer (Effect.Effect Unit)

bindBuffer :: WebGLContext -> GLenum -> WebGLBuffer -> Effect.Effect Unit
bindBuffer webgl target buffer = runFn3 bindBufferImpl webgl target buffer

foreign import bindFramebufferImpl :: Fn3 WebGLContext GLenum WebGLFramebuffer (Effect.Effect Unit)

bindFramebuffer :: WebGLContext -> GLenum -> WebGLFramebuffer -> Effect.Effect Unit
bindFramebuffer webgl target framebuffer = runFn3 bindFramebufferImpl webgl target framebuffer

foreign import bindRenderbufferImpl :: Fn3 WebGLContext GLenum WebGLRenderbuffer (Effect.Effect Unit)

bindRenderbuffer :: WebGLContext -> GLenum -> WebGLRenderbuffer -> Effect.Effect Unit
bindRenderbuffer webgl target renderbuffer = runFn3 bindRenderbufferImpl webgl target renderbuffer

foreign import bindTextureImpl :: Fn3 WebGLContext GLenum WebGLTexture (Effect.Effect Unit)

bindTexture :: WebGLContext -> GLenum -> WebGLTexture -> Effect.Effect Unit
bindTexture webgl target texture = runFn3 bindTextureImpl webgl target texture

foreign import blendColorImpl :: Fn5 WebGLContext GLclampf GLclampf GLclampf GLclampf (Effect.Effect Unit)

blendColor :: WebGLContext -> GLclampf -> GLclampf -> GLclampf -> GLclampf -> Effect.Effect Unit
blendColor webgl red green blue alpha = runFn5 blendColorImpl webgl red green blue alpha

foreign import blendEquationImpl :: Fn2 WebGLContext GLenum (Effect.Effect Unit)

blendEquation :: WebGLContext -> GLenum -> Effect.Effect Unit
blendEquation webgl mode = runFn2 blendEquationImpl webgl mode

foreign import blendEquationSeparateImpl :: Fn3 WebGLContext GLenum GLenum (Effect.Effect Unit)

blendEquationSeparate :: WebGLContext -> GLenum -> GLenum -> Effect.Effect Unit
blendEquationSeparate webgl modeRGB modeAlpha = runFn3 blendEquationSeparateImpl webgl modeRGB modeAlpha

foreign import blendFuncImpl :: Fn3 WebGLContext GLenum GLenum (Effect.Effect Unit)

blendFunc :: WebGLContext -> GLenum -> GLenum -> Effect.Effect Unit
blendFunc webgl sfactor dfactor = runFn3 blendFuncImpl webgl sfactor dfactor

foreign import blendFuncSeparateImpl :: Fn5 WebGLContext GLenum GLenum GLenum GLenum (Effect.Effect Unit)

blendFuncSeparate :: WebGLContext -> GLenum -> GLenum -> GLenum -> GLenum -> Effect.Effect Unit
blendFuncSeparate webgl srcRGB dstRGB srcAlpha dstAlpha = runFn5 blendFuncSeparateImpl webgl srcRGB dstRGB srcAlpha dstAlpha

foreign import bufferDataImpl :: Fn4 WebGLContext GLenum BufferDataSource GLenum (Effect.Effect Unit)

bufferData :: WebGLContext -> GLenum -> BufferDataSource -> GLenum -> Effect.Effect Unit
bufferData webgl target data' usage = runFn4 bufferDataImpl webgl target data' usage

foreign import bufferData_Impl :: Fn4 WebGLContext GLenum GLsizeiptr GLenum (Effect.Effect Unit)

bufferData_ :: WebGLContext -> GLenum -> GLsizeiptr -> GLenum -> Effect.Effect Unit
bufferData_ webgl target size usage = runFn4 bufferData_Impl webgl target size usage

foreign import bufferSubDataImpl :: Fn4 WebGLContext GLenum GLintptr BufferDataSource (Effect.Effect Unit)

bufferSubData :: WebGLContext -> GLenum -> GLintptr -> BufferDataSource -> Effect.Effect Unit
bufferSubData webgl target offset data' = runFn4 bufferSubDataImpl webgl target offset data'

foreign import checkFramebufferStatusImpl :: Fn2 WebGLContext GLenum (Effect.Effect GLenum)

checkFramebufferStatus :: WebGLContext -> GLenum -> Effect.Effect GLenum
checkFramebufferStatus webgl target = runFn2 checkFramebufferStatusImpl webgl target

foreign import clearImpl :: Fn2 WebGLContext GLbitfield (Effect.Effect Unit)

clear :: WebGLContext -> GLbitfield -> Effect.Effect Unit
clear webgl mask = runFn2 clearImpl webgl mask

foreign import clearColorImpl :: Fn5 WebGLContext GLclampf GLclampf GLclampf GLclampf (Effect.Effect Unit)

clearColor :: WebGLContext -> GLclampf -> GLclampf -> GLclampf -> GLclampf -> Effect.Effect Unit
clearColor webgl red green blue alpha = runFn5 clearColorImpl webgl red green blue alpha

foreign import clearDepthImpl :: Fn2 WebGLContext GLclampf (Effect.Effect Unit)

clearDepth :: WebGLContext -> GLclampf -> Effect.Effect Unit
clearDepth webgl depth = runFn2 clearDepthImpl webgl depth

foreign import clearStencilImpl :: Fn2 WebGLContext GLint (Effect.Effect Unit)

clearStencil :: WebGLContext -> GLint -> Effect.Effect Unit
clearStencil webgl s = runFn2 clearStencilImpl webgl s

foreign import colorMaskImpl :: Fn5 WebGLContext GLboolean GLboolean GLboolean GLboolean (Effect.Effect Unit)

colorMask :: WebGLContext -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> Effect.Effect Unit
colorMask webgl red green blue alpha = runFn5 colorMaskImpl webgl red green blue alpha

foreign import compileShaderImpl :: Fn2 WebGLContext WebGLShader (Effect.Effect Unit)

compileShader :: WebGLContext -> WebGLShader -> Effect.Effect Unit
compileShader webgl shader = runFn2 compileShaderImpl webgl shader

foreign import compressedTexImage2DImpl :: Fn8 WebGLContext GLenum GLint GLenum GLsizei GLsizei GLint ArrayBufferView (Effect.Effect Unit)

compressedTexImage2D :: WebGLContext -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> ArrayBufferView -> Effect.Effect Unit
compressedTexImage2D webgl target level internalformat width height border data' = runFn8 compressedTexImage2DImpl webgl target level internalformat width height border data'

foreign import compressedTexSubImage2DImpl :: Fn9 WebGLContext GLenum GLint GLint GLint GLsizei GLsizei GLenum ArrayBufferView (Effect.Effect Unit)

compressedTexSubImage2D :: WebGLContext -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> ArrayBufferView -> Effect.Effect Unit
compressedTexSubImage2D webgl target level xoffset yoffset width height format data' = runFn9 compressedTexSubImage2DImpl webgl target level xoffset yoffset width height format data'

foreign import copyTexImage2DImpl :: Fn9 WebGLContext GLenum GLint GLenum GLint GLint GLsizei GLsizei GLint (Effect.Effect Unit)

copyTexImage2D :: WebGLContext -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> Effect.Effect Unit
copyTexImage2D webgl target level internalformat x y width height border = runFn9 copyTexImage2DImpl webgl target level internalformat x y width height border

foreign import copyTexSubImage2DImpl :: Fn9 WebGLContext GLenum GLint GLint GLint GLint GLint GLsizei GLsizei (Effect.Effect Unit)

copyTexSubImage2D :: WebGLContext -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> Effect.Effect Unit
copyTexSubImage2D webgl target level xoffset yoffset x y width height = runFn9 copyTexSubImage2DImpl webgl target level xoffset yoffset x y width height

foreign import createBufferImpl :: Fn1 WebGLContext (Effect.Effect WebGLBuffer)

createBuffer :: WebGLContext -> Effect.Effect (Maybe WebGLBuffer)
createBuffer webgl = runFn1 createBufferImpl webgl >>= Lib.Maybe.from >>> pure

foreign import createFramebufferImpl :: Fn1 WebGLContext (Effect.Effect WebGLFramebuffer)

createFramebuffer :: WebGLContext -> Effect.Effect (Maybe WebGLFramebuffer)
createFramebuffer webgl = runFn1 createFramebufferImpl webgl >>= Lib.Maybe.from >>> pure

foreign import createProgramImpl :: Fn1 WebGLContext (Effect.Effect WebGLProgram)

createProgram :: WebGLContext -> Effect.Effect (Maybe WebGLProgram)
createProgram webgl = runFn1 createProgramImpl webgl >>= Lib.Maybe.from >>> pure

foreign import createRenderbufferImpl :: Fn1 WebGLContext (Effect.Effect WebGLRenderbuffer)

createRenderbuffer :: WebGLContext -> Effect.Effect (Maybe WebGLRenderbuffer)
createRenderbuffer webgl = runFn1 createRenderbufferImpl webgl >>= Lib.Maybe.from >>> pure

foreign import createShaderImpl :: Fn2 WebGLContext GLenum (Effect.Effect WebGLShader)

createShader :: WebGLContext -> GLenum -> Effect.Effect (Maybe WebGLShader)
createShader webgl type' = runFn2 createShaderImpl webgl type' >>= Lib.Maybe.from >>> pure

foreign import createTextureImpl :: Fn1 WebGLContext (Effect.Effect WebGLTexture)

createTexture :: WebGLContext -> Effect.Effect (Maybe WebGLTexture)
createTexture webgl = runFn1 createTextureImpl webgl >>= Lib.Maybe.from >>> pure

foreign import cullFaceImpl :: Fn2 WebGLContext GLenum (Effect.Effect Unit)

cullFace :: WebGLContext -> GLenum -> Effect.Effect Unit
cullFace webgl mode = runFn2 cullFaceImpl webgl mode

foreign import deleteBufferImpl :: Fn2 WebGLContext WebGLBuffer (Effect.Effect Unit)

deleteBuffer :: WebGLContext -> WebGLBuffer -> Effect.Effect Unit
deleteBuffer webgl buffer = runFn2 deleteBufferImpl webgl buffer

foreign import deleteFramebufferImpl :: Fn2 WebGLContext WebGLFramebuffer (Effect.Effect Unit)

deleteFramebuffer :: WebGLContext -> WebGLFramebuffer -> Effect.Effect Unit
deleteFramebuffer webgl framebuffer = runFn2 deleteFramebufferImpl webgl framebuffer

foreign import deleteProgramImpl :: Fn2 WebGLContext WebGLProgram (Effect.Effect Unit)

deleteProgram :: WebGLContext -> WebGLProgram -> Effect.Effect Unit
deleteProgram webgl program = runFn2 deleteProgramImpl webgl program

foreign import deleteRenderbufferImpl :: Fn2 WebGLContext WebGLRenderbuffer (Effect.Effect Unit)

deleteRenderbuffer :: WebGLContext -> WebGLRenderbuffer -> Effect.Effect Unit
deleteRenderbuffer webgl renderbuffer = runFn2 deleteRenderbufferImpl webgl renderbuffer

foreign import deleteShaderImpl :: Fn2 WebGLContext WebGLShader (Effect.Effect Unit)

deleteShader :: WebGLContext -> WebGLShader -> Effect.Effect Unit
deleteShader webgl shader = runFn2 deleteShaderImpl webgl shader

foreign import deleteTextureImpl :: Fn2 WebGLContext WebGLTexture (Effect.Effect Unit)

deleteTexture :: WebGLContext -> WebGLTexture -> Effect.Effect Unit
deleteTexture webgl texture = runFn2 deleteTextureImpl webgl texture

foreign import depthFuncImpl :: Fn2 WebGLContext GLenum (Effect.Effect Unit)

depthFunc :: WebGLContext -> GLenum -> Effect.Effect Unit
depthFunc webgl func = runFn2 depthFuncImpl webgl func

foreign import depthMaskImpl :: Fn2 WebGLContext GLboolean (Effect.Effect Unit)

depthMask :: WebGLContext -> GLboolean -> Effect.Effect Unit
depthMask webgl flag = runFn2 depthMaskImpl webgl flag

foreign import depthRangeImpl :: Fn3 WebGLContext GLclampf GLclampf (Effect.Effect Unit)

depthRange :: WebGLContext -> GLclampf -> GLclampf -> Effect.Effect Unit
depthRange webgl zNear zFar = runFn3 depthRangeImpl webgl zNear zFar

foreign import detachShaderImpl :: Fn3 WebGLContext WebGLProgram WebGLShader (Effect.Effect Unit)

detachShader :: WebGLContext -> WebGLProgram -> WebGLShader -> Effect.Effect Unit
detachShader webgl program shader = runFn3 detachShaderImpl webgl program shader

foreign import disableImpl :: Fn2 WebGLContext GLenum (Effect.Effect Unit)

disable :: WebGLContext -> GLenum -> Effect.Effect Unit
disable webgl cap = runFn2 disableImpl webgl cap

foreign import disableVertexAttribArrayImpl :: Fn2 WebGLContext GLuint (Effect.Effect Unit)

disableVertexAttribArray :: WebGLContext -> GLuint -> Effect.Effect Unit
disableVertexAttribArray webgl index = runFn2 disableVertexAttribArrayImpl webgl index

foreign import drawArraysImpl :: Fn4 WebGLContext GLenum GLint GLsizei (Effect.Effect Unit)

drawArrays :: WebGLContext -> GLenum -> GLint -> GLsizei -> Effect.Effect Unit
drawArrays webgl mode first count = runFn4 drawArraysImpl webgl mode first count

foreign import drawElementsImpl :: Fn5 WebGLContext GLenum GLsizei GLenum GLintptr (Effect.Effect Unit)

drawElements :: WebGLContext -> GLenum -> GLsizei -> GLenum -> GLintptr -> Effect.Effect Unit
drawElements webgl mode count type' offset = runFn5 drawElementsImpl webgl mode count type' offset

foreign import enableImpl :: Fn2 WebGLContext GLenum (Effect.Effect Unit)

enable :: WebGLContext -> GLenum -> Effect.Effect Unit
enable webgl cap = runFn2 enableImpl webgl cap

foreign import enableVertexAttribArrayImpl :: Fn2 WebGLContext GLuint (Effect.Effect Unit)

enableVertexAttribArray :: WebGLContext -> GLuint -> Effect.Effect Unit
enableVertexAttribArray webgl index = runFn2 enableVertexAttribArrayImpl webgl index

foreign import finishImpl :: Fn1 WebGLContext (Effect.Effect Unit)

finish :: WebGLContext -> Effect.Effect Unit
finish webgl = runFn1 finishImpl webgl

foreign import flushImpl :: Fn1 WebGLContext (Effect.Effect Unit)

flush :: WebGLContext -> Effect.Effect Unit
flush webgl = runFn1 flushImpl webgl

foreign import framebufferRenderbufferImpl :: Fn5 WebGLContext GLenum GLenum GLenum WebGLRenderbuffer (Effect.Effect Unit)

framebufferRenderbuffer :: WebGLContext -> GLenum -> GLenum -> GLenum -> WebGLRenderbuffer -> Effect.Effect Unit
framebufferRenderbuffer webgl target attachment renderbuffertarget renderbuffer = runFn5 framebufferRenderbufferImpl webgl target attachment renderbuffertarget renderbuffer

foreign import framebufferTexture2DImpl :: Fn6 WebGLContext GLenum GLenum GLenum WebGLTexture GLint (Effect.Effect Unit)

framebufferTexture2D :: WebGLContext -> GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> Effect.Effect Unit
framebufferTexture2D webgl target attachment textarget texture level = runFn6 framebufferTexture2DImpl webgl target attachment textarget texture level

foreign import frontFaceImpl :: Fn2 WebGLContext GLenum (Effect.Effect Unit)

frontFace :: WebGLContext -> GLenum -> Effect.Effect Unit
frontFace webgl mode = runFn2 frontFaceImpl webgl mode

foreign import generateMipmapImpl :: Fn2 WebGLContext GLenum (Effect.Effect Unit)

generateMipmap :: WebGLContext -> GLenum -> Effect.Effect Unit
generateMipmap webgl target = runFn2 generateMipmapImpl webgl target

foreign import getActiveAttribImpl :: Fn3 WebGLContext WebGLProgram GLuint (Effect.Effect WebGLActiveInfo)

getActiveAttrib :: WebGLContext -> WebGLProgram -> GLuint -> Effect.Effect (Maybe WebGLActiveInfo)
getActiveAttrib webgl program index = runFn3 getActiveAttribImpl webgl program index >>= Lib.Maybe.from >>> pure

foreign import getActiveUniformImpl :: Fn3 WebGLContext WebGLProgram GLuint (Effect.Effect WebGLActiveInfo)

getActiveUniform :: WebGLContext -> WebGLProgram -> GLuint -> Effect.Effect (Maybe WebGLActiveInfo)
getActiveUniform webgl program index = runFn3 getActiveUniformImpl webgl program index >>= Lib.Maybe.from >>> pure

foreign import getAttachedShadersImpl :: Fn2 WebGLContext WebGLProgram (Effect.Effect (Array WebGLShader))

getAttachedShaders :: WebGLContext -> WebGLProgram -> Effect.Effect (Array WebGLShader)
getAttachedShaders webgl program = runFn2 getAttachedShadersImpl webgl program >>= nullAsEmpty >>> pure

foreign import getAttribLocationImpl :: Fn3 WebGLContext WebGLProgram DOMString (Effect.Effect GLint)

getAttribLocation :: WebGLContext -> WebGLProgram -> DOMString -> Effect.Effect GLint
getAttribLocation webgl program name = runFn3 getAttribLocationImpl webgl program name

foreign import getBufferParameterImpl :: forall eff a. Fn3 WebGLContext GLenum GLenum (Effect.Effect a)

getBufferParameter :: forall eff a. WebGLContext -> GLenum -> GLenum -> Effect.Effect (Maybe a)
getBufferParameter webgl target pname = runFn3 getBufferParameterImpl webgl target pname >>= Lib.Maybe.from >>> pure

foreign import getContextAttributesImpl :: Fn1 WebGLContext (Effect.Effect WebGLContextAttributes)

getContextAttributes :: WebGLContext -> Effect.Effect (Maybe WebGLContextAttributes)
getContextAttributes webgl = runFn1 getContextAttributesImpl webgl >>= Lib.Maybe.from >>> pure

foreign import getErrorImpl :: Fn1 WebGLContext (Effect.Effect GLenum)

getError :: WebGLContext -> Effect.Effect GLenum
getError webgl = runFn1 getErrorImpl webgl

foreign import getExtensionImpl :: forall eff a. Fn2 WebGLContext DOMString (Effect.Effect a)

getExtension :: forall eff a. WebGLContext -> DOMString -> Effect.Effect (Maybe a)
getExtension webgl name = runFn2 getExtensionImpl webgl name >>= Lib.Maybe.from >>> pure

foreign import getFramebufferAttachmentParameterImpl :: forall eff a. Fn4 WebGLContext GLenum GLenum GLenum (Effect.Effect a)

getFramebufferAttachmentParameter :: forall eff a. WebGLContext -> GLenum -> GLenum -> GLenum -> Effect.Effect (Maybe a)
getFramebufferAttachmentParameter webgl target attachment pname = runFn4 getFramebufferAttachmentParameterImpl webgl target attachment pname >>= Lib.Maybe.from >>> pure

foreign import getParameterImpl :: forall eff a. Fn2 WebGLContext GLenum (Effect.Effect a)

getParameter :: forall eff a. WebGLContext -> GLenum -> Effect.Effect (Maybe a)
getParameter webgl pname = runFn2 getParameterImpl webgl pname >>= Lib.Maybe.from >>> pure

foreign import getProgramInfoLogImpl :: Fn2 WebGLContext WebGLProgram (Effect.Effect DOMString)

getProgramInfoLog :: WebGLContext -> WebGLProgram -> Effect.Effect (Maybe DOMString)
getProgramInfoLog webgl program = runFn2 getProgramInfoLogImpl webgl program >>= Lib.Maybe.from >>> pure

foreign import getProgramParameterImpl :: forall eff a. Fn3 WebGLContext WebGLProgram GLenum (Effect.Effect a)

getProgramParameter :: forall eff a. WebGLContext -> WebGLProgram -> GLenum -> Effect.Effect (Maybe a)
getProgramParameter webgl program pname = runFn3 getProgramParameterImpl webgl program pname >>= Lib.Maybe.from >>> pure

foreign import getRenderbufferParameterImpl :: forall eff a. Fn3 WebGLContext GLenum GLenum (Effect.Effect a)

getRenderbufferParameter :: forall eff a. WebGLContext -> GLenum -> GLenum -> Effect.Effect (Maybe a)
getRenderbufferParameter webgl target pname = runFn3 getRenderbufferParameterImpl webgl target pname >>= Lib.Maybe.from >>> pure

foreign import getShaderInfoLogImpl :: Fn2 WebGLContext WebGLShader (Effect.Effect DOMString)

getShaderInfoLog :: WebGLContext -> WebGLShader -> Effect.Effect (Maybe DOMString)
getShaderInfoLog webgl shader = runFn2 getShaderInfoLogImpl webgl shader >>= Lib.Maybe.from >>> pure

foreign import getShaderParameterImpl :: forall eff a. Fn3 WebGLContext WebGLShader GLenum (Effect.Effect a)

getShaderParameter :: forall a. WebGLContext -> WebGLShader -> GLenum -> Effect.Effect (Maybe a)
getShaderParameter webgl shader pname = runFn3 getShaderParameterImpl webgl shader pname >>= Lib.Maybe.from >>> pure

foreign import getShaderPrecisionFormatImpl :: Fn3 WebGLContext GLenum GLenum (Effect.Effect WebGLShaderPrecisionFormat)

getShaderPrecisionFormat :: WebGLContext -> GLenum -> GLenum -> Effect.Effect (Maybe WebGLShaderPrecisionFormat)
getShaderPrecisionFormat webgl shadertype precisiontype = runFn3 getShaderPrecisionFormatImpl webgl shadertype precisiontype >>= Lib.Maybe.from >>> pure

foreign import getShaderSourceImpl :: Fn2 WebGLContext WebGLShader (Effect.Effect DOMString)

getShaderSource :: WebGLContext -> WebGLShader -> Effect.Effect (Maybe DOMString)
getShaderSource webgl shader = runFn2 getShaderSourceImpl webgl shader >>= Lib.Maybe.from >>> pure

foreign import getSupportedExtensionsImpl :: Fn1 WebGLContext (Effect.Effect (Array DOMString))

getSupportedExtensions :: WebGLContext -> Effect.Effect (Array DOMString)
getSupportedExtensions webgl = runFn1 getSupportedExtensionsImpl webgl >>= nullAsEmpty >>> pure

foreign import getTexParameterImpl :: forall eff a. Fn3 WebGLContext GLenum GLenum (Effect.Effect a)

getTexParameter :: forall eff a. WebGLContext -> GLenum -> GLenum -> Effect.Effect (Maybe a)
getTexParameter webgl target pname = runFn3 getTexParameterImpl webgl target pname >>= Lib.Maybe.from >>> pure

foreign import getUniformImpl :: forall eff a. Fn3 WebGLContext WebGLProgram WebGLUniformLocation (Effect.Effect a)

getUniform :: forall eff a. WebGLContext -> WebGLProgram -> WebGLUniformLocation -> Effect.Effect (Maybe a)
getUniform webgl program location = runFn3 getUniformImpl webgl program location >>= Lib.Maybe.from >>> pure

foreign import getUniformLocationImpl :: Fn3 WebGLContext WebGLProgram DOMString (Effect.Effect WebGLUniformLocation)

getUniformLocation :: WebGLContext -> WebGLProgram -> DOMString -> Effect.Effect (Maybe WebGLUniformLocation)
getUniformLocation webgl program name = runFn3 getUniformLocationImpl webgl program name >>= Lib.Maybe.from >>> pure

foreign import getVertexAttribImpl :: forall eff a. Fn3 WebGLContext GLuint GLenum (Effect.Effect a)

getVertexAttrib :: forall eff a. WebGLContext -> GLuint -> GLenum -> Effect.Effect (Maybe a)
getVertexAttrib webgl index pname = runFn3 getVertexAttribImpl webgl index pname >>= Lib.Maybe.from >>> pure

foreign import getVertexAttribOffsetImpl :: Fn3 WebGLContext GLuint GLenum (Effect.Effect GLsizeiptr)

getVertexAttribOffset :: WebGLContext -> GLuint -> GLenum -> Effect.Effect GLsizeiptr
getVertexAttribOffset webgl index pname = runFn3 getVertexAttribOffsetImpl webgl index pname

foreign import hintImpl :: Fn3 WebGLContext GLenum GLenum (Effect.Effect Unit)

hint :: WebGLContext -> GLenum -> GLenum -> Effect.Effect Unit
hint webgl target mode = runFn3 hintImpl webgl target mode

foreign import isBufferImpl :: Fn2 WebGLContext WebGLBuffer (Effect.Effect GLboolean)

isBuffer :: WebGLContext -> WebGLBuffer -> Effect.Effect GLboolean
isBuffer webgl buffer = runFn2 isBufferImpl webgl buffer

foreign import isContextLostImpl :: Fn1 WebGLContext (Effect.Effect Boolean)

isContextLost :: WebGLContext -> Effect.Effect Boolean
isContextLost webgl = runFn1 isContextLostImpl webgl

foreign import isEnabledImpl :: Fn2 WebGLContext GLenum (Effect.Effect GLboolean)

isEnabled :: WebGLContext -> GLenum -> Effect.Effect GLboolean
isEnabled webgl cap = runFn2 isEnabledImpl webgl cap

foreign import isFramebufferImpl :: Fn2 WebGLContext WebGLFramebuffer (Effect.Effect GLboolean)

isFramebuffer :: WebGLContext -> WebGLFramebuffer -> Effect.Effect GLboolean
isFramebuffer webgl framebuffer = runFn2 isFramebufferImpl webgl framebuffer

foreign import isProgramImpl :: Fn2 WebGLContext WebGLProgram (Effect.Effect GLboolean)

isProgram :: WebGLContext -> WebGLProgram -> Effect.Effect GLboolean
isProgram webgl program = runFn2 isProgramImpl webgl program

foreign import isRenderbufferImpl :: Fn2 WebGLContext WebGLRenderbuffer (Effect.Effect GLboolean)

isRenderbuffer :: WebGLContext -> WebGLRenderbuffer -> Effect.Effect GLboolean
isRenderbuffer webgl renderbuffer = runFn2 isRenderbufferImpl webgl renderbuffer

foreign import isShaderImpl :: Fn2 WebGLContext WebGLShader (Effect.Effect GLboolean)

isShader :: WebGLContext -> WebGLShader -> Effect.Effect GLboolean
isShader webgl shader = runFn2 isShaderImpl webgl shader

foreign import isTextureImpl :: Fn2 WebGLContext WebGLTexture (Effect.Effect GLboolean)

isTexture :: WebGLContext -> WebGLTexture -> Effect.Effect GLboolean
isTexture webgl texture = runFn2 isTextureImpl webgl texture

foreign import lineWidthImpl :: Fn2 WebGLContext GLfloat (Effect.Effect Unit)

lineWidth :: WebGLContext -> GLfloat -> Effect.Effect Unit
lineWidth webgl width = runFn2 lineWidthImpl webgl width

foreign import linkProgramImpl :: Fn2 WebGLContext WebGLProgram (Effect.Effect Unit)

linkProgram :: WebGLContext -> WebGLProgram -> Effect.Effect Unit
linkProgram webgl program = runFn2 linkProgramImpl webgl program

foreign import pixelStoreiImpl :: Fn3 WebGLContext GLenum GLint (Effect.Effect Unit)

pixelStorei :: WebGLContext -> GLenum -> GLint -> Effect.Effect Unit
pixelStorei webgl pname param = runFn3 pixelStoreiImpl webgl pname param

foreign import polygonOffsetImpl :: Fn3 WebGLContext GLfloat GLfloat (Effect.Effect Unit)

polygonOffset :: WebGLContext -> GLfloat -> GLfloat -> Effect.Effect Unit
polygonOffset webgl factor units = runFn3 polygonOffsetImpl webgl factor units

foreign import readPixelsImpl :: Fn8 WebGLContext GLint GLint GLsizei GLsizei GLenum GLenum ArrayBufferView (Effect.Effect Unit)

readPixels :: WebGLContext -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> ArrayBufferView -> Effect.Effect Unit
readPixels webgl x y width height format type' pixels = runFn8 readPixelsImpl webgl x y width height format type' pixels

foreign import renderbufferStorageImpl :: Fn5 WebGLContext GLenum GLenum GLsizei GLsizei (Effect.Effect Unit)

renderbufferStorage :: WebGLContext -> GLenum -> GLenum -> GLsizei -> GLsizei -> Effect.Effect Unit
renderbufferStorage webgl target internalformat width height = runFn5 renderbufferStorageImpl webgl target internalformat width height

foreign import sampleCoverageImpl :: Fn3 WebGLContext GLclampf GLboolean (Effect.Effect Unit)

sampleCoverage :: WebGLContext -> GLclampf -> GLboolean -> Effect.Effect Unit
sampleCoverage webgl value invert = runFn3 sampleCoverageImpl webgl value invert

foreign import scissorImpl :: Fn5 WebGLContext GLint GLint GLsizei GLsizei (Effect.Effect Unit)

scissor :: WebGLContext -> GLint -> GLint -> GLsizei -> GLsizei -> Effect.Effect Unit
scissor webgl x y width height = runFn5 scissorImpl webgl x y width height

foreign import shaderSourceImpl :: Fn3 WebGLContext WebGLShader DOMString (Effect.Effect Unit)

shaderSource :: WebGLContext -> WebGLShader -> DOMString -> Effect.Effect Unit
shaderSource webgl shader source = runFn3 shaderSourceImpl webgl shader source

foreign import stencilFuncImpl :: Fn4 WebGLContext GLenum GLint GLuint (Effect.Effect Unit)

stencilFunc :: WebGLContext -> GLenum -> GLint -> GLuint -> Effect.Effect Unit
stencilFunc webgl func ref mask = runFn4 stencilFuncImpl webgl func ref mask

foreign import stencilFuncSeparateImpl :: Fn5 WebGLContext GLenum GLenum GLint GLuint (Effect.Effect Unit)

stencilFuncSeparate :: WebGLContext -> GLenum -> GLenum -> GLint -> GLuint -> Effect.Effect Unit
stencilFuncSeparate webgl face func ref mask = runFn5 stencilFuncSeparateImpl webgl face func ref mask

foreign import stencilMaskImpl :: Fn2 WebGLContext GLuint (Effect.Effect Unit)

stencilMask :: WebGLContext -> GLuint -> Effect.Effect Unit
stencilMask webgl mask = runFn2 stencilMaskImpl webgl mask

foreign import stencilMaskSeparateImpl :: Fn3 WebGLContext GLenum GLuint (Effect.Effect Unit)

stencilMaskSeparate :: WebGLContext -> GLenum -> GLuint -> Effect.Effect Unit
stencilMaskSeparate webgl face mask = runFn3 stencilMaskSeparateImpl webgl face mask

foreign import stencilOpImpl :: Fn4 WebGLContext GLenum GLenum GLenum (Effect.Effect Unit)

stencilOp :: WebGLContext -> GLenum -> GLenum -> GLenum -> Effect.Effect Unit
stencilOp webgl fail zfail zpass = runFn4 stencilOpImpl webgl fail zfail zpass

foreign import stencilOpSeparateImpl :: Fn5 WebGLContext GLenum GLenum GLenum GLenum (Effect.Effect Unit)

stencilOpSeparate :: WebGLContext -> GLenum -> GLenum -> GLenum -> GLenum -> Effect.Effect Unit
stencilOpSeparate webgl face fail zfail zpass = runFn5 stencilOpSeparateImpl webgl face fail zfail zpass

foreign import texImage2DImpl :: Fn7 WebGLContext GLenum GLint GLenum GLenum GLenum TexImageSource (Effect.Effect Unit)

texImage2D :: WebGLContext -> GLenum -> GLint -> GLenum -> GLenum -> GLenum -> TexImageSource -> Effect.Effect Unit
texImage2D webgl target level internalformat format type' source = runFn7 texImage2DImpl webgl target level internalformat format type' source

foreign import texImage2D_Impl :: Fn10 WebGLContext GLenum GLint GLenum GLsizei GLsizei GLint GLenum GLenum ArrayBufferView (Effect.Effect Unit)

texImage2D_ :: WebGLContext -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> ArrayBufferView -> Effect.Effect Unit
texImage2D_ webgl target level internalformat width height border format type' pixels = runFn10 texImage2D_Impl webgl target level internalformat width height border format type' pixels

foreign import texParameterfImpl :: Fn4 WebGLContext GLenum GLenum GLfloat (Effect.Effect Unit)

texParameterf :: WebGLContext -> GLenum -> GLenum -> GLfloat -> Effect.Effect Unit
texParameterf webgl target pname param = runFn4 texParameterfImpl webgl target pname param

foreign import texParameteriImpl :: Fn4 WebGLContext GLenum GLenum GLint (Effect.Effect Unit)

texParameteri :: WebGLContext -> GLenum -> GLenum -> GLint -> Effect.Effect Unit
texParameteri webgl target pname param = runFn4 texParameteriImpl webgl target pname param

foreign import texSubImage2DImpl :: Fn8 WebGLContext GLenum GLint GLint GLint GLenum GLenum TexImageSource (Effect.Effect Unit)

texSubImage2D :: WebGLContext -> GLenum -> GLint -> GLint -> GLint -> GLenum -> GLenum -> TexImageSource -> Effect.Effect Unit
texSubImage2D webgl target level xoffset yoffset format type' source = runFn8 texSubImage2DImpl webgl target level xoffset yoffset format type' source

foreign import texSubImage2D_Impl :: Fn10 WebGLContext GLenum GLint GLint GLint GLsizei GLsizei GLenum GLenum ArrayBufferView (Effect.Effect Unit)

texSubImage2D_ :: WebGLContext -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> ArrayBufferView -> Effect.Effect Unit
texSubImage2D_ webgl target level xoffset yoffset width height format type' pixels = runFn10 texSubImage2D_Impl webgl target level xoffset yoffset width height format type' pixels

foreign import uniform1fImpl :: Fn3 WebGLContext WebGLUniformLocation GLfloat (Effect.Effect Unit)

uniform1f :: WebGLContext -> WebGLUniformLocation -> GLfloat -> Effect.Effect Unit
uniform1f webgl location x = runFn3 uniform1fImpl webgl location x

foreign import uniform1fvImpl :: Fn3 WebGLContext WebGLUniformLocation (Array GLfloat) (Effect.Effect Unit)

uniform1fv :: WebGLContext -> WebGLUniformLocation -> (Array GLfloat) -> Effect.Effect Unit
uniform1fv webgl location v = runFn3 uniform1fvImpl webgl location v

foreign import uniform1fv_Impl :: Fn3 WebGLContext WebGLUniformLocation Float32Array (Effect.Effect Unit)

uniform1fv_ :: WebGLContext -> WebGLUniformLocation -> Float32Array -> Effect.Effect Unit
uniform1fv_ webgl location v = runFn3 uniform1fv_Impl webgl location v

foreign import uniform1iImpl :: Fn3 WebGLContext WebGLUniformLocation GLint (Effect.Effect Unit)

uniform1i :: WebGLContext -> WebGLUniformLocation -> GLint -> Effect.Effect Unit
uniform1i webgl location x = runFn3 uniform1iImpl webgl location x

foreign import uniform1ivImpl :: Fn3 WebGLContext WebGLUniformLocation (Array Int) (Effect.Effect Unit)

uniform1iv :: WebGLContext -> WebGLUniformLocation -> (Array Int) -> Effect.Effect Unit
uniform1iv webgl location v = runFn3 uniform1ivImpl webgl location v

foreign import uniform1iv_Impl :: Fn3 WebGLContext WebGLUniformLocation Int32Array (Effect.Effect Unit)

uniform1iv_ :: WebGLContext -> WebGLUniformLocation -> Int32Array -> Effect.Effect Unit
uniform1iv_ webgl location v = runFn3 uniform1iv_Impl webgl location v

foreign import uniform2fImpl :: Fn4 WebGLContext WebGLUniformLocation GLfloat GLfloat (Effect.Effect Unit)

uniform2f :: WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> Effect.Effect Unit
uniform2f webgl location x y = runFn4 uniform2fImpl webgl location x y

foreign import uniform2fvImpl :: Fn3 WebGLContext WebGLUniformLocation (Array GLfloat) (Effect.Effect Unit)

uniform2fv :: WebGLContext -> WebGLUniformLocation -> (Array GLfloat) -> Effect.Effect Unit
uniform2fv webgl location v = runFn3 uniform2fvImpl webgl location v

foreign import uniform2fv_Impl :: Fn3 WebGLContext WebGLUniformLocation Float32Array (Effect.Effect Unit)

uniform2fv_ :: WebGLContext -> WebGLUniformLocation -> Float32Array -> Effect.Effect Unit
uniform2fv_ webgl location v = runFn3 uniform2fv_Impl webgl location v

foreign import uniform2iImpl :: Fn4 WebGLContext WebGLUniformLocation GLint GLint (Effect.Effect Unit)

uniform2i :: WebGLContext -> WebGLUniformLocation -> GLint -> GLint -> Effect.Effect Unit
uniform2i webgl location x y = runFn4 uniform2iImpl webgl location x y

foreign import uniform2ivImpl :: Fn3 WebGLContext WebGLUniformLocation (Array Int) (Effect.Effect Unit)

uniform2iv :: WebGLContext -> WebGLUniformLocation -> (Array Int) -> Effect.Effect Unit
uniform2iv webgl location v = runFn3 uniform2ivImpl webgl location v

foreign import uniform2iv_Impl :: Fn3 WebGLContext WebGLUniformLocation Int32Array (Effect.Effect Unit)

uniform2iv_ :: WebGLContext -> WebGLUniformLocation -> Int32Array -> Effect.Effect Unit
uniform2iv_ webgl location v = runFn3 uniform2iv_Impl webgl location v

foreign import uniform3fImpl :: Fn5 WebGLContext WebGLUniformLocation GLfloat GLfloat GLfloat (Effect.Effect Unit)

uniform3f :: WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> Effect.Effect Unit
uniform3f webgl location x y z = runFn5 uniform3fImpl webgl location x y z

foreign import uniform3fvImpl :: Fn3 WebGLContext WebGLUniformLocation (Array GLfloat) (Effect.Effect Unit)

uniform3fv :: WebGLContext -> WebGLUniformLocation -> (Array GLfloat) -> Effect.Effect Unit
uniform3fv webgl location v = runFn3 uniform3fvImpl webgl location v

foreign import uniform3fv_Impl :: Fn3 WebGLContext WebGLUniformLocation Float32Array (Effect.Effect Unit)

uniform3fv_ :: WebGLContext -> WebGLUniformLocation -> Float32Array -> Effect.Effect Unit
uniform3fv_ webgl location v = runFn3 uniform3fv_Impl webgl location v

foreign import uniform3iImpl :: Fn5 WebGLContext WebGLUniformLocation GLint GLint GLint (Effect.Effect Unit)

uniform3i :: WebGLContext -> WebGLUniformLocation -> GLint -> GLint -> GLint -> Effect.Effect Unit
uniform3i webgl location x y z = runFn5 uniform3iImpl webgl location x y z

foreign import uniform3ivImpl :: Fn3 WebGLContext WebGLUniformLocation (Array Int) (Effect.Effect Unit)

uniform3iv :: WebGLContext -> WebGLUniformLocation -> (Array Int) -> Effect.Effect Unit
uniform3iv webgl location v = runFn3 uniform3ivImpl webgl location v

foreign import uniform3iv_Impl :: Fn3 WebGLContext WebGLUniformLocation Int32Array (Effect.Effect Unit)

uniform3iv_ :: WebGLContext -> WebGLUniformLocation -> Int32Array -> Effect.Effect Unit
uniform3iv_ webgl location v = runFn3 uniform3iv_Impl webgl location v

foreign import uniform4fImpl :: Fn6 WebGLContext WebGLUniformLocation GLfloat GLfloat GLfloat GLfloat (Effect.Effect Unit)

uniform4f :: WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Effect.Effect Unit
uniform4f webgl location x y z w = runFn6 uniform4fImpl webgl location x y z w

foreign import uniform4fvImpl :: Fn3 WebGLContext WebGLUniformLocation (Array GLfloat) (Effect.Effect Unit)

uniform4fv :: WebGLContext -> WebGLUniformLocation -> (Array GLfloat) -> Effect.Effect Unit
uniform4fv webgl location v = runFn3 uniform4fvImpl webgl location v

foreign import uniform4fv_Impl :: Fn3 WebGLContext WebGLUniformLocation Float32Array (Effect.Effect Unit)

uniform4fv_ :: WebGLContext -> WebGLUniformLocation -> Float32Array -> Effect.Effect Unit
uniform4fv_ webgl location v = runFn3 uniform4fv_Impl webgl location v

foreign import uniform4iImpl :: Fn6 WebGLContext WebGLUniformLocation GLint GLint GLint GLint (Effect.Effect Unit)

uniform4i :: WebGLContext -> WebGLUniformLocation -> GLint -> GLint -> GLint -> GLint -> Effect.Effect Unit
uniform4i webgl location x y z w = runFn6 uniform4iImpl webgl location x y z w

foreign import uniform4ivImpl :: Fn3 WebGLContext WebGLUniformLocation (Array Int) (Effect.Effect Unit)

uniform4iv :: WebGLContext -> WebGLUniformLocation -> (Array Int) -> Effect.Effect Unit
uniform4iv webgl location v = runFn3 uniform4ivImpl webgl location v

foreign import uniform4iv_Impl :: Fn3 WebGLContext WebGLUniformLocation Int32Array (Effect.Effect Unit)

uniform4iv_ :: WebGLContext -> WebGLUniformLocation -> Int32Array -> Effect.Effect Unit
uniform4iv_ webgl location v = runFn3 uniform4iv_Impl webgl location v

foreign import uniformMatrix2fvImpl :: Fn4 WebGLContext WebGLUniformLocation GLboolean (Array GLfloat) (Effect.Effect Unit)

uniformMatrix2fv :: WebGLContext -> WebGLUniformLocation -> GLboolean -> (Array GLfloat) -> Effect.Effect Unit
uniformMatrix2fv webgl location transpose value = runFn4 uniformMatrix2fvImpl webgl location transpose value

foreign import uniformMatrix2fv_Impl :: Fn4 WebGLContext WebGLUniformLocation GLboolean Float32Array (Effect.Effect Unit)

uniformMatrix2fv_ :: WebGLContext -> WebGLUniformLocation -> GLboolean -> Float32Array -> Effect.Effect Unit
uniformMatrix2fv_ webgl location transpose value = runFn4 uniformMatrix2fv_Impl webgl location transpose value

foreign import uniformMatrix3fvImpl :: Fn4 WebGLContext WebGLUniformLocation GLboolean (Array GLfloat) (Effect.Effect Unit)

uniformMatrix3fv :: WebGLContext -> WebGLUniformLocation -> GLboolean -> (Array GLfloat) -> Effect.Effect Unit
uniformMatrix3fv webgl location transpose value = runFn4 uniformMatrix3fvImpl webgl location transpose value

foreign import uniformMatrix3fv_Impl :: Fn4 WebGLContext WebGLUniformLocation GLboolean Float32Array (Effect.Effect Unit)

uniformMatrix3fv_ :: WebGLContext -> WebGLUniformLocation -> GLboolean -> Float32Array -> Effect.Effect Unit
uniformMatrix3fv_ webgl location transpose value = runFn4 uniformMatrix3fv_Impl webgl location transpose value

foreign import uniformMatrix4fvImpl :: Fn4 WebGLContext WebGLUniformLocation GLboolean (Array GLfloat) (Effect.Effect Unit)

uniformMatrix4fv :: WebGLContext -> WebGLUniformLocation -> GLboolean -> (Array GLfloat) -> Effect.Effect Unit
uniformMatrix4fv webgl location transpose value = runFn4 uniformMatrix4fvImpl webgl location transpose value

foreign import uniformMatrix4fv_Impl :: Fn4 WebGLContext WebGLUniformLocation GLboolean Float32Array (Effect.Effect Unit)

uniformMatrix4fv_ :: WebGLContext -> WebGLUniformLocation -> GLboolean -> Float32Array -> Effect.Effect Unit
uniformMatrix4fv_ webgl location transpose value = runFn4 uniformMatrix4fv_Impl webgl location transpose value

foreign import useProgramImpl :: Fn2 WebGLContext WebGLProgram (Effect.Effect Unit)

useProgram :: WebGLContext -> WebGLProgram -> Effect.Effect Unit
useProgram webgl program = runFn2 useProgramImpl webgl program

foreign import validateProgramImpl :: Fn2 WebGLContext WebGLProgram (Effect.Effect Unit)

validateProgram :: WebGLContext -> WebGLProgram -> Effect.Effect Unit
validateProgram webgl program = runFn2 validateProgramImpl webgl program

foreign import vertexAttrib1fImpl :: Fn3 WebGLContext GLuint GLfloat (Effect.Effect Unit)

vertexAttrib1f :: WebGLContext -> GLuint -> GLfloat -> Effect.Effect Unit
vertexAttrib1f webgl indx x = runFn3 vertexAttrib1fImpl webgl indx x

foreign import vertexAttrib1fvImpl :: Fn3 WebGLContext GLuint (Array GLfloat) (Effect.Effect Unit)

vertexAttrib1fv :: WebGLContext -> GLuint -> (Array GLfloat) -> Effect.Effect Unit
vertexAttrib1fv webgl indx values = runFn3 vertexAttrib1fvImpl webgl indx values

foreign import vertexAttrib1fv_Impl :: Fn3 WebGLContext GLuint Float32Array (Effect.Effect Unit)

vertexAttrib1fv_ :: WebGLContext -> GLuint -> Float32Array -> Effect.Effect Unit
vertexAttrib1fv_ webgl indx values = runFn3 vertexAttrib1fv_Impl webgl indx values

foreign import vertexAttrib2fImpl :: Fn4 WebGLContext GLuint GLfloat GLfloat (Effect.Effect Unit)

vertexAttrib2f :: WebGLContext -> GLuint -> GLfloat -> GLfloat -> Effect.Effect Unit
vertexAttrib2f webgl indx x y = runFn4 vertexAttrib2fImpl webgl indx x y

foreign import vertexAttrib2fvImpl :: Fn3 WebGLContext GLuint (Array GLfloat) (Effect.Effect Unit)

vertexAttrib2fv :: WebGLContext -> GLuint -> (Array GLfloat) -> Effect.Effect Unit
vertexAttrib2fv webgl indx values = runFn3 vertexAttrib2fvImpl webgl indx values

foreign import vertexAttrib2fv_Impl :: Fn3 WebGLContext GLuint Float32Array (Effect.Effect Unit)

vertexAttrib2fv_ :: WebGLContext -> GLuint -> Float32Array -> Effect.Effect Unit
vertexAttrib2fv_ webgl indx values = runFn3 vertexAttrib2fv_Impl webgl indx values

foreign import vertexAttrib3fImpl :: Fn5 WebGLContext GLuint GLfloat GLfloat GLfloat (Effect.Effect Unit)

vertexAttrib3f :: WebGLContext -> GLuint -> GLfloat -> GLfloat -> GLfloat -> Effect.Effect Unit
vertexAttrib3f webgl indx x y z = runFn5 vertexAttrib3fImpl webgl indx x y z

foreign import vertexAttrib3fvImpl :: Fn3 WebGLContext GLuint (Array GLfloat) (Effect.Effect Unit)

vertexAttrib3fv :: WebGLContext -> GLuint -> (Array GLfloat) -> Effect.Effect Unit
vertexAttrib3fv webgl indx values = runFn3 vertexAttrib3fvImpl webgl indx values

foreign import vertexAttrib3fv_Impl :: Fn3 WebGLContext GLuint Float32Array (Effect.Effect Unit)

vertexAttrib3fv_ :: WebGLContext -> GLuint -> Float32Array -> Effect.Effect Unit
vertexAttrib3fv_ webgl indx values = runFn3 vertexAttrib3fv_Impl webgl indx values

foreign import vertexAttrib4fImpl :: Fn6 WebGLContext GLuint GLfloat GLfloat GLfloat GLfloat (Effect.Effect Unit)

vertexAttrib4f :: WebGLContext -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Effect.Effect Unit
vertexAttrib4f webgl indx x y z w = runFn6 vertexAttrib4fImpl webgl indx x y z w

foreign import vertexAttrib4fvImpl :: Fn3 WebGLContext GLuint (Array GLfloat) (Effect.Effect Unit)

vertexAttrib4fv :: WebGLContext -> GLuint -> (Array GLfloat) -> Effect.Effect Unit
vertexAttrib4fv webgl indx values = runFn3 vertexAttrib4fvImpl webgl indx values

foreign import vertexAttrib4fv_Impl :: Fn3 WebGLContext GLuint Float32Array (Effect.Effect Unit)

vertexAttrib4fv_ :: WebGLContext -> GLuint -> Float32Array -> Effect.Effect Unit
vertexAttrib4fv_ webgl indx values = runFn3 vertexAttrib4fv_Impl webgl indx values

foreign import vertexAttribPointerImpl :: Fn7 WebGLContext GLuint GLint GLenum GLboolean GLsizei GLintptr (Effect.Effect Unit)

vertexAttribPointer :: WebGLContext -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> Effect.Effect Unit
vertexAttribPointer webgl indx size type' normalized stride offset = runFn7 vertexAttribPointerImpl webgl indx size type' normalized stride offset

foreign import viewportImpl :: Fn5 WebGLContext GLint GLint GLsizei GLsizei (Effect.Effect Unit)

viewport :: WebGLContext -> GLint -> GLint -> GLsizei -> GLsizei -> Effect.Effect Unit
viewport webgl x y width height = runFn5 viewportImpl webgl x y width height

foreign import getCanvasImpl :: WebGLContext -> Effect.Effect Graphics.Canvas.CanvasElement

getCanvas :: WebGLContext -> Effect.Effect Graphics.Canvas.CanvasElement
getCanvas = getCanvasImpl

