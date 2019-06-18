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
import qualified DialogFlow.Response as DFR

import DialogFlow.Payload.Google
import qualified DialogFlow.Payload.Google.Response as GR
import qualified DialogFlow.Payload.Google.OtherTypes as GO


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

type API = "fulfillment" :> ReqBody '[JSON] DFRequest :> Post '[JSON] DFR.Response

fulfillment :: DFRequest -> Handler DFR.Response
fulfillment req = do
  liftIO $ putStrLn "Request!"
  types <- liftIO $ pokeApiRequest req
  case types of
    Left err -> error "SomeException"
    Right types ->
      return $ createResponse types

createResponse :: [Type'] -> DFR.Response
createResponse types =
  let types' = fmap getTypeName types
      msg = T.unpack $ T.intercalate " and " types'
      speechResponse = SimpleResponse (TextToSpeech msg) Nothing
      image = GO.Image "https://avatars0.githubusercontent.com/u/180308" "desc" Nothing Nothing
      cardContent = GO.BasicCardImage image
      basicCard = GR.BasicCard (Just "Le title") (Just "Le subtitle") cardContent [] GO.DEFAULT
      -- imageUri = "https://avatars0.githubusercontent.com/u/180308?s=460&v=4"
      -- basicCard = Message (BasicCard (Just "Title") Nothing (BasicCardImage msg) [])
      -- card = Message (Card (Just "Result mate") (Just "Poke-result") (Just imageUri) [])
      response = GR.Response False [GR.RichResponse $ GR.SimpleResponse speechResponse, GR.RichResponse  basicCard]
      payload = GooglePayload response
   in DFR.Response (Just msg) [Message $ SimpleResponses [speechResponse]] (Just "mauriciofierro.dev") payload
   -- in Response (Just msg) [Message (SimpleResponses [speechResponse])] (Just "mauriciofierro.dev")

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
