module Main where

import Lib
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  putStrLn "Starting server..."
  run 8081 app
