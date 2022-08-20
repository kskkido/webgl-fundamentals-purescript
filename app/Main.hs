module Main where

import RIO
import qualified System.IO as IO
import qualified Applications.Server.Main as Server

main :: IO.IO ()
main = Server.main
