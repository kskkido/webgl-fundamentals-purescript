module Lib.WebGL.Models.Texture2dParameters.Main where

import Lib.Window.WebGL.Types as Lib.Window.WebGL.Types

type Texture2dParameters =
  { level :: Int
  , internalFormat :: Int
  , format :: Int
  , type :: Int
  , source :: Lib.Window.WebGL.Types.TexImageSource
  }
