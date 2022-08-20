module Applications.Server.Models.Env.Main
  ( Env(..)
  , fromConfig
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Applications.Server.Models.Config.Main as Config

data Env = Env
  { stage :: Text
  , serverPort :: Int
  }

fromConfig :: Config.Config -> Env
fromConfig cx = Env
  { stage = Text.pack cx.stage
  , serverPort = cx.serverPort
  }
