{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    (
     app
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Debug.Trace
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import PokeApi.Types
import PokeApi.Type.Types
import PokeApi.Type.Queries
import PokeApi.Pokemon.Queries
import Servant
import Servant.Client hiding (Response)
import Types

import qualified Data.Map as M
import qualified Data.Text as T

import Dialogflow.Message
import Dialogflow.Response (Response(..))

import qualified Dialogflow.Payload.Google as G
import Dialogflow.Request


extractTypeParameter ::  Request -> Maybe Type'
extractTypeParameter req = do
  typeParam <- (M.lookup "PokemonType" . parameters . queryResult) req
  getType (T.pack typeParam)

extractQualifierParameter :: Request -> Maybe Qualifier
extractQualifierParameter  req = do
  typeParam <- (M.lookup "Quality" . parameters . queryResult) req
  getQualifier typeParam
    where
      getQualifier :: String -> Maybe Qualifier
      getQualifier "weak" = Just Weak
      getQualifier "effective" = Just Effective
      getQualifier _ = Nothing

extractGameParameter :: Request -> Maybe String
extractGameParameter = M.lookup "PokemonGameVersion" . parameters . queryResult

type API = "fulfillment" :> ReqBody '[JSON] Request :> Post '[JSON] Response

fulfillment :: Request -> Handler Response
fulfillment req = do
  liftIO $ putStrLn "Request!"
  case (intent . queryResult) req of
    Nothing -> error "No intent" -- TODO: this should obviously not throw an error.
    Just intent -> fulfillIntent req (displayName intent)

fulfillIntent :: Request -> String -> Handler Response
fulfillIntent req = \case
  "Get types" -> do
    types <- liftIO $ pokeApiRequest req
    case types of
      Left err -> error "SomeException"
      Right types ->
        return $ createResponse types
  "Get Pokemon location" ->
    let msg = "In what game?"
        speechResponse = SimpleResponse (TextToSpeech msg) Nothing
        response = G.Response True [G.RichResponse $ G.SimpleResponse speechResponse]
        payload = G.GooglePayload response
     in return $ Response (Just msg) [Message $ SimpleResponses [speechResponse]] (Just "mauriciofierro.dev") payload Nothing Nothing
  "Get Pokemon location - custom" -> do
    games <- liftIO $ gameLocationRequest req
    case games of
      Left err -> error "Error"
      Right games -> return $ createFollowupResponse games


createFollowupResponse :: [String] -> Response
createFollowupResponse encounters =
  let msg = intercalate " and " encounters
      speechResponse = SimpleResponse (TextToSpeech msg) Nothing
      response = G.Response False [G.RichResponse $ G.SimpleResponse speechResponse]
      payload = G.GooglePayload response
   in Response (Just msg) [Message $ SimpleResponses [speechResponse]] (Just "mauriciofierro.dev") payload Nothing Nothing

createResponse :: [Type'] -> Response
createResponse types =
  let types' = fmap getTypeName types
      msg = T.unpack $ T.intercalate " and " types'
      speechResponse = SimpleResponse (TextToSpeech msg) Nothing
      image = G.Image "https://avatars0.githubusercontent.com/u/180308" "desc" Nothing Nothing
      cardContent = G.BasicCardImage image
      basicCard = G.BasicCard (Just "Le title") (Just "Le subtitle") cardContent [] G.DEFAULT
      response = G.Response False [G.RichResponse $ G.SimpleResponse speechResponse, G.RichResponse  basicCard]
      payload = G.GooglePayload response
   in Response (Just msg) [Message $ SimpleResponses [speechResponse]] (Just "mauriciofierro.dev") payload Nothing Nothing

pokeApiRequest :: Request -> PokeApi [Type']
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


gameLocationRequest :: Request -> PokeApi [String]
gameLocationRequest req = do
  manager' <- liftIO $ newManager tlsManagerSettings
  case extractGameParameter req of
    Just game -> do
      traceShowM game
      case Dialogflow.Request.outputContexts (queryResult req) of
        Just ctxs ->
          case getContextParam ctxs (session req <> "/contexts/getpokemonlocation-followup") "Pokemon" of
            Just pkmnName ->
              let clientEnv = mkClientEnv manager' (BaseUrl Https "pokeapi.co" 443 "/api/v2")
               in pokemonEncounterByGame clientEnv pkmnName game
            _ -> error "No Pokemon context"
        _ -> error "No contexts"
    _ -> error "No game"

server :: Server API
server = fulfillment

fulFillmentAPI :: Proxy API
fulFillmentAPI = Proxy

app :: Application
app = serve fulFillmentAPI server
