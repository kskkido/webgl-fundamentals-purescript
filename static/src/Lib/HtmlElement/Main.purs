module Lib.HtmlElement.Main where

import Prelude
import Effect as Effect
import Control.Monad.Maybe.Trans as MaybeT
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.DOM.Document as Web.DOM.Document
import Web.DOM.Element as Web.DOM.Element
import Lib.Element.Main as Lib.Element

fromTag :: String -> Web.HTML.Window.Window -> MaybeT.MaybeT Effect.Effect Web.HTML.HTMLElement.HTMLElement
fromTag tag window = MaybeT.MaybeT $ Web.HTML.HTMLElement.fromElement <$> Lib.Element.fromTag tag window

