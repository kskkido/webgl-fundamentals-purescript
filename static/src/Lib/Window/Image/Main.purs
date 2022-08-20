module Lib.Window.Image.Main where

import Prelude
import Effect as Effect
import Effect.Aff as Effect.Aff
import Effect.Aff.Compat as Effect.Aff.Compat
import Data.Either as Either
import Lib.Window.WebGL.Types as Lib.Window.WebGL.Types

foreign import data Image :: Type

foreign import fromUrlImpl :: String -> Effect.Aff.Compat.EffectFnAff Image

fromUrl :: String -> Effect.Aff.Aff (Either.Either String Image)
fromUrl url = do
  image <- Effect.Aff.Compat.fromEffectFnAff $ fromUrlImpl url
  pure $ Either.Right image
  `Effect.Aff.catchError` \_ -> do
    pure $ Either.Left $ "Unable to load image from " <> url

foreign import toTexImageSourceImpl :: Image -> Lib.Window.WebGL.Types.TexImageSource

toTexImageSource :: Image -> Lib.Window.WebGL.Types.TexImageSource
toTexImageSource = toTexImageSourceImpl

foreign import getWidthImpl :: Image -> Effect.Effect Number

getWidth :: Image -> Effect.Effect Number
getWidth = getWidthImpl

foreign import getHeightImpl :: Image -> Effect.Effect Number

getHeight :: Image -> Effect.Effect Number
getHeight = getHeightImpl
