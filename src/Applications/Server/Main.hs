module Applications.Server.Main
  ( main
  ) where

import RIO
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified Lucid
import qualified Web.Scotty as Scotty
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai.Middleware.Static as Static
import qualified Applications.Server.Models.Env.Main as Models.Env
import qualified Applications.Server.Models.Config.Main as Models.Config
import qualified Applications.Server.Templates.Pages.Home.Main as Pages.Home
import qualified Applications.Server.Templates.Pages.Shaders.Animation.Main as Pages.Shaders.Animation
import qualified Applications.Server.Templates.Pages.Shaders.Camera.Main as Pages.Shaders.Camera
import qualified Applications.Server.Templates.Pages.Shaders.CameraLookAt.Main as Pages.Shaders.CameraLookAt
import qualified Applications.Server.Templates.Pages.Shaders.Fudge.Main as Pages.Shaders.Fudge
import qualified Applications.Server.Templates.Pages.Shaders.LightingDirectional.Main as Pages.Shaders.LightingDirectional
import qualified Applications.Server.Templates.Pages.Shaders.LightingPoint.Main as Pages.Shaders.LightingPoint
import qualified Applications.Server.Templates.Pages.Shaders.Rectangle.Main as Pages.Shaders.Rectangle
import qualified Applications.Server.Templates.Pages.Shaders.Perspective.Main as Pages.Shaders.Perspective
import qualified Applications.Server.Templates.Pages.Shaders.Shape3d.Main as Pages.Shaders.Shape3d
import qualified Applications.Server.Templates.Pages.Shaders.Texture2d.Main as Pages.Shaders.Texture2d

main :: IO.IO ()
main = do
  config <- Models.Config.fromSystem >>= either (IO.Error.ioError . IO.Error.userError) return
  let env = Models.Env.fromConfig config
  Scotty.scotty env.serverPort do
    Scotty.middleware Cors.simpleCors
    Scotty.middleware $ Static.staticPolicy $ Static.addBase "static/public"
    Scotty.get "/" do
      Scotty.html $ Lucid.renderText Pages.Home.render
    Scotty.get "/shaders/animation" do
      Scotty.html $ Lucid.renderText Pages.Shaders.Animation.render
    Scotty.get "/shaders/camera" do
      Scotty.html $ Lucid.renderText Pages.Shaders.Camera.render
    Scotty.get "/shaders/camera-look-at" do
      Scotty.html $ Lucid.renderText Pages.Shaders.CameraLookAt.render
    Scotty.get "/shaders/fudge" do
      Scotty.html $ Lucid.renderText Pages.Shaders.Fudge.render
    Scotty.get "/shaders/lighting-directional" do
      Scotty.html $ Lucid.renderText Pages.Shaders.LightingDirectional.render
    Scotty.get "/shaders/lighting-point" do
      Scotty.html $ Lucid.renderText Pages.Shaders.LightingPoint.render
    Scotty.get "/shaders/rectangle" do
      Scotty.html $ Lucid.renderText Pages.Shaders.Rectangle.render
    Scotty.get "/shaders/perspective" do
      Scotty.html $ Lucid.renderText Pages.Shaders.Perspective.render
    Scotty.get "/shaders/shape3d" do
      Scotty.html $ Lucid.renderText Pages.Shaders.Shape3d.render
    Scotty.get "/shaders/texture2d" do
      Scotty.html $ Lucid.renderText Pages.Shaders.Texture2d.render

