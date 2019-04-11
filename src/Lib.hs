{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( someFunc
    ) where

import Servant
import Servant.API

import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type API = "fulfillment" :> ReqBody '[JSON] DFRequest :> Post '[JSON] DFResponse

fulfillment :: DFRequest -> Handler DFResponse
fulfillment = undefined

server :: Server API
server = fulfillment
