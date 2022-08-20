module Applications.Server.Templates.Pages.Home.Main
  ( render
  ) where

import RIO
import qualified Lucid

render :: Lucid.Html ()
render = Lucid.doctypehtml_ do
  Lucid.head_ do
    Lucid.title_ "home"
    Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "styles/index.css"]
    Lucid.script_ [Lucid.src_ "https://unpkg.com/htmx.org@1.7.0"] ""
    Lucid.script_ [Lucid.src_ "https://unpkg.com/hyperscript.org@0.9.5"] ""
    Lucid.script_ [Lucid.src_ "https://kit.fontawesome.com/56e0eb8f60.js", Lucid.crossorigin_ "anonymous"] ""
  Lucid.body_ [Lucid.id_ "root"] do
    Lucid.ul_ [] do
      Lucid.li_ [] do
        Lucid.a_ [Lucid.href_ "/shaders/animation"] do
          "animation"
      Lucid.li_ [] do
        Lucid.a_ [Lucid.href_ "/shaders/camera"] do
          "camera"
      Lucid.li_ [] do
        Lucid.a_ [Lucid.href_ "/shaders/camera-look-at"] do
          "camera-look-at"
      Lucid.li_ [] do
        Lucid.a_ [Lucid.href_ "/shaders/fudge"] do
          "fudge"
      Lucid.li_ [] do
        Lucid.a_ [Lucid.href_ "/shaders/lighting-directional"] do
          "lighting-directional"
      Lucid.li_ [] do
        Lucid.a_ [Lucid.href_ "/shaders/rectangle"] do
          "rectangle"
      Lucid.li_ [] do
        Lucid.a_ [Lucid.href_ "/shaders/perspective"] do
          "perspective"
      Lucid.li_ [] do
        Lucid.a_ [Lucid.href_ "/shaders/shape3d"] do
          "shape3d"
      Lucid.li_ [] do
        Lucid.a_ [Lucid.href_ "/shaders/texture2d"] do
          "texture2d"

