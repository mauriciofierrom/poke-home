module Main where

import Lib
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp
import Servant.Client

main :: IO ()
main = do
  manager' <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager' (BaseUrl Https "pokeapi.co" 443 "/api/v2")
   in do
    putStrLn "Starting server..."
    run 8081 (app clientEnv)
