{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( someFunc
    , app
    ) where

import Servant
import Servant.API
import Network.Wai.Handler.Warp

import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type API = "fulfillment" :> ReqBody '[JSON] DFRequest :> Post '[JSON] DFResponse

fulfillment :: DFRequest -> Handler DFResponse
fulfillment = undefined

server :: Server API
server = fulfillment

fulFillmentAPI :: Proxy API
fulFillmentAPI = Proxy

app :: Application
app = serve fulFillmentAPI server
