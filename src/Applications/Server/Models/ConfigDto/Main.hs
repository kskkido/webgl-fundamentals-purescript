module Applications.Server.Models.ConfigDto.Main
  ( ConfigDto(..)
  , fromSystem
  ) where

import RIO
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Except as Except

data ConfigDto = ConfigDto
  { stage :: String
  , serverPort :: String
  } deriving (Eq, Show)

fromSystem :: IO.IO (Either String ConfigDto)
fromSystem = do
  env <- Maybe.runMaybeT do
    stage            <- Maybe.MaybeT $ Environment.lookupEnv "STAGE"
    serverPort       <- Maybe.MaybeT $ Environment.lookupEnv "SERVER_PORT"
    return $ ConfigDto
      { stage = stage
      , serverPort = serverPort
      }
  return $ maybe (Left "Unable to parse System Env") Right env

