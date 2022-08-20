module Applications.Server.Templates.Pages.Shaders.Animation.Main
  ( render
  ) where

import RIO
import qualified Lucid

render :: Lucid.Html ()
render = Lucid.doctypehtml_ do
  Lucid.head_ do
    Lucid.title_ "shaders/animation"
    Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "/styles/index.css"]
    Lucid.script_ [Lucid.src_ "/scripts/pages/shaders/animation/index.js", Lucid.defer_ ""] ""
    Lucid.script_ [Lucid.src_ "https://unpkg.com/htmx.org@1.7.0"] ""
    Lucid.script_ [Lucid.src_ "https://unpkg.com/hyperscript.org@0.9.5"] ""
    Lucid.script_ [Lucid.src_ "https://kit.fontawesome.com/56e0eb8f60.js", Lucid.crossorigin_ "anonymous"] ""
  Lucid.body_ [Lucid.id_ "root"] do
    Lucid.canvas_ [Lucid.id_ "canvas", Lucid.class_ "canvas"] mempty


