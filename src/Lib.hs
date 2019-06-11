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
import Types

import qualified Data.Map as M
import qualified Data.Text as T

import DialogFlow.Message
import DialogFlow.Response


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

type API = "fulfillment" :> ReqBody '[JSON] DFRequest :> Post '[JSON] Response

fulfillment :: DFRequest -> Handler Response
fulfillment req = do
  types <- liftIO $ pokeApiRequest req
  case types of
    Left err -> error "SomeException"
    Right types ->
      return $ createResponse types

createResponse :: [Type'] -> Response
createResponse types =
  let types' = fmap getTypeName types
      msg = T.unpack $ T.intercalate " and " types'
      speechResponse = SimpleResponse (TextToSpeech msg) Nothing
      payload = GooglePayload False [speechResponse]
   in Response (Just msg) [Message (SimpleResponses [speechResponse])] (Just "mauriciofierro.dev") payload

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
