module Lib.Element.Main where

import Prelude
import Effect as Effect
import Data.Maybe as Maybe
import Data.Traversable as Traversable
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.DOM.Node as Web.DOM.Node
import Web.DOM.Document as Web.DOM.Document
import Web.DOM.DOMTokenList as Web.DOM.DOMTokenList
import Web.DOM.Element as Web.DOM.Element

fromTag :: String -> Web.HTML.Window.Window -> Effect.Effect Web.DOM.Element.Element
fromTag tag window = do
  document <- Web.HTML.HTMLDocument.toDocument <$> Web.HTML.Window.document window
  Web.DOM.Document.createElement tag document

addClass :: String -> Web.DOM.Element.Element -> Effect.Effect Unit
addClass name element = do
  classList <- Web.DOM.Element.classList element
  Web.DOM.DOMTokenList.add classList name

append :: Array Web.DOM.Node.Node -> Web.DOM.Element.Element -> Effect.Effect Unit
append nodes element = do
  let parent = Web.DOM.Element.toNode element
  flip Traversable.traverse_ nodes $ \node -> Web.DOM.Node.appendChild node parent

setTextContent :: String -> Web.DOM.Element.Element -> Effect.Effect Unit
setTextContent text element = do
  let parent = Web.DOM.Element.toNode element
  Web.DOM.Node.setTextContent text parent
