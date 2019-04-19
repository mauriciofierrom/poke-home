{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    (
     app
    ) where

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import PokeApi.Types
import PokeApi.Type.Types
import PokeApi.Type.Queries
import Servant
import Servant.API
import Types
import DialogFlow.Message.Types

import qualified Data.Text as T
import qualified Data.Map as M

extractTypeParameter ::  DFRequest -> Maybe Type'
extractTypeParameter req = do
  typeParam <- (M.lookup "PokemonType" . parameters . queryResult) req
  getType (T.pack typeParam)

extractQualifierParameter :: DFRequest -> Maybe Qualifier
extractQualifierParameter  req = do
  typeParam <- (M.lookup "Quality" . parameters . queryResult) req
  getQualifier typeParam
    where
      getQualifier :: String -> Maybe Qualifier
      getQualifier "weak" = Just Weak
      getQualifier "effective" = Just Effective
      getQualifier _ = Nothing

type API = "fulfillment" :> ReqBody '[JSON] DFRequest :> Post '[JSON] DFResponse

fulfillment :: DFRequest -> Handler DFResponse
fulfillment req = do
  types <- liftIO $ pokeApiRequest req
  case types of
    Left err -> error "SomeException"
    Right types ->
      return $ createResponse types

createResponse :: [Type'] -> DFResponse
createResponse types =
  let types' = fmap getTypeName types
      msg = T.unpack $ T.intercalate " and " types'
      payload = GooglePayload False [SimpleResponse (Just msg)]
   in DFResponse (Just msg) [FulfillmentMessage (MSimpleResponses [MSimpleResponse msg])] (Just "mauriciofierro.dev") payload

pokeApiRequest :: DFRequest -> PokeApi [Type']
pokeApiRequest req =
  let typeParam = extractTypeParameter req
      qualifierParam = extractQualifierParameter req
   in
     case (typeParam, qualifierParam) of
       (Just type', Just qualifier) -> do
         manager <- liftIO $ newManager tlsManagerSettings
         case qualifier of
           Effective -> effectiveAgainst manager type'
           Weak -> weakAgainst manager type'

server :: Server API
server = fulfillment

fulFillmentAPI :: Proxy API
fulFillmentAPI = Proxy

app :: Application
app = serve fulFillmentAPI server
