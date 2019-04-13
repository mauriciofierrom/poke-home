{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( 
     app
    ) where

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import PokeApi.Type.Types
import Servant
import Servant.API
import Types

import qualified Data.Text as T
import qualified Data.Map as M

extractTypeParameter ::  DFRequest -> Maybe Type'
extractTypeParameter req = do
  typeParam <- (M.lookup "PokemonType" . parameters . queryResult) req
  getType (T.pack typeParam)

extractQualifierParameter :: DFRequest -> Maybe Qualifier
extractQualifierParameter  req = do
  typeParam <- (M.lookup "Qualifier" . parameters . queryResult) req
  getQualifier typeParam
    where
      getQualifier :: String -> Maybe Qualifier
      getQualifier "weak" = Just Weak
      getQualifier "effective" = Just Effective
      getQualifier _ = Nothing

type API = "fulfillment" :> ReqBody '[JSON] DFRequest :> Post '[JSON] DFResponse

fulfillment :: DFRequest -> Handler DFResponse
fulfillment = undefined

server :: Server API
server = fulfillment

fulFillmentAPI :: Proxy API
fulFillmentAPI = Proxy

app :: Application
app = serve fulFillmentAPI server
