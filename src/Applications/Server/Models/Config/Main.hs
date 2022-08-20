module Applications.Server.Models.Config.Main
  ( Config(..)
  , fromSystem
  , fromConfigDto
  ) where

import RIO
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Except as Except
import qualified Lib.Parse.Main as Lib.Parse
import qualified Applications.Server.Models.ConfigDto.Main as ConfigDto

data Config = Config
  { stage :: String
  , serverPort :: Int
  } deriving (Eq, Show)

fromSystem :: IO.IO (Either String Config)
fromSystem = do
  dto <- ConfigDto.fromSystem
  return $ dto >>= fromConfigDto

fromConfigDto :: ConfigDto.ConfigDto -> Either String Config
fromConfigDto dto = do
  serverPort <- maybe (Left "unable to parse server port") Right $ Lib.Parse.parseInt dto.serverPort
  return Config
    { stage = dto.stage
    , serverPort = serverPort
    }

