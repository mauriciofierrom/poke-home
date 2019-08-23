{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}

module Lib
    (
     app
    ) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception hiding (Handler)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import PokeApi.Types
import PokeApi.Type.Types
import PokeApi.Type.Queries
import PokeApi.Pokemon.Queries
import PokeApi.Pokemon.Types
import Servant
import Servant.Client hiding (Response)
import Types

import qualified Data.Map as M
import qualified Data.Text as T

import Dialogflow.V2.Fulfillment.Message
import Dialogflow.V2.Fulfillment.Webhook.Response hiding (outputContexts)

import qualified Dialogflow.V2.Fulfillment.Payload.Google as G
import Dialogflow.V2.Fulfillment.Webhook.Request


extractTypeParameter ::  WebhookRequest -> Maybe Type'
extractTypeParameter req = do
  typeParam <- (M.lookup "PokemonType" . parameters . queryResult) req
  getType (T.pack typeParam)

extractQualifierParameter :: WebhookRequest -> Maybe Qualifier
extractQualifierParameter  req = do
  typeParam <- (M.lookup "Quality" . parameters . queryResult) req
  getQualifier typeParam
    where
      getQualifier :: String -> Maybe Qualifier
      getQualifier "weak" = Just Weak
      getQualifier "effective" = Just Effective
      getQualifier _ = Nothing

extractGameParameter :: WebhookRequest -> Maybe String
extractGameParameter = M.lookup "PokemonGameVersion" . parameters . queryResult

type API = "fulfillment" :> ReqBody '[JSON] WebhookRequest :> Post '[JSON] WebhookResponse

fulfillment :: WebhookRequest -> Handler WebhookResponse
fulfillment req = do
  liftIO $ putStrLn "WebhookRequest!"
  case (intent . queryResult) req of
    Nothing -> error "No intent" -- TODO: this should obviously not throw an error.
    Just intent -> fulfillIntent req (displayName intent)

-- fulfill :: WebhookRequest -> Handler WebhookResponse
-- fulfill req = do
--   liftIO $ putStrLn "WebhookRequest!"
--   case (intent . queryResult) req of
--     Nothing -> error "No intent"
--     Just intent -> fulfillIt req (displayName intent)

fulfillIntent :: WebhookRequest -> String -> Handler WebhookResponse
fulfillIntent req = \case
  "Get types" -> do
    types <- liftIO $ pokeApiWebhookRequest req
    case types of
      Left err -> error "SomeException"
      Right types ->
        return $ createResponse types
  "Get Pokemon location" ->
    let msg = "In what game?"
        speechResponse = SimpleResponse (TextToSpeech msg) Nothing
        response = G.Response True Nothing (G.RichResponse  [G.Item $ G.SimpleResponse speechResponse] [] Nothing)
        payload = Just $ G.GooglePayload response
     in return $ WebhookResponse (Just msg) (Just [Message $ SimpleResponses [speechResponse]]) (Just "mauriciofierro.dev") payload Nothing Nothing
  "Get Pokemon location - custom" -> do
    games <- liftIO $ gameLocationWebhookRequest req
    case games of
      Left err -> error "Error"
      Right games -> return $ createFollowupResponse games

fulfillIt
  :: HasPokeApi m
  => WebhookRequest
  -> String
  -> m WebhookResponse
fulfillIt req = \case
  "Get types" -> do
    types <- pokeApiRequest req
    return $ createResponse types

createFollowupResponse :: [String] -> WebhookResponse
createFollowupResponse encounters =
  let msg = intercalate " and " encounters
      speechResponse = SimpleResponse (TextToSpeech msg) Nothing
      response = G.Response False Nothing (G.RichResponse [G.Item $ G.SimpleResponse speechResponse] [] Nothing)
      payload = Just $ G.GooglePayload response
   in WebhookResponse (Just msg) (Just [Message $ SimpleResponses [speechResponse]]) (Just "mauriciofierro.dev") payload Nothing Nothing

createResponse :: [Type'] -> WebhookResponse
createResponse types =
  let types' = fmap getTypeName types
      msg = T.unpack $ T.intercalate " and " types'
      speechResponse = SimpleResponse (TextToSpeech msg) (Just msg)
      image = G.Image "https://avatars0.githubusercontent.com/u/180308" "desc" Nothing Nothing
      cardContent = G.BasicCardImage image
      basicCard = G.BasicCard (Just "Le title") (Just "Le subtitle") cardContent [] G.DEFAULT
      response = G.Response False Nothing (G.RichResponse [G.Item $ G.SimpleResponse speechResponse, G.Item basicCard] [] Nothing)
      payload = Just $ G.GooglePayload response
   in WebhookResponse (Just msg) (Just [Message $ SimpleResponses [speechResponse]]) (Just "mauriciofierro.dev") payload Nothing Nothing

pokeApiWebhookRequest :: WebhookRequest -> PokeApi [Type']
pokeApiWebhookRequest req =
  let typeParam = extractTypeParameter req
      qualifierParam = extractQualifierParameter req
   in
     case (typeParam, qualifierParam) of
       (Just type', Just qualifier) -> do
         manager <- liftIO $ newManager tlsManagerSettings
         case qualifier of
           Effective -> effectiveAgainst manager type'
           Weak -> weakAgainst manager type'

pokeApiRequest
  :: HasPokeApi m
  => WebhookRequest
  -> m [Type']
pokeApiRequest req =
  let typeParam = extractTypeParameter req
      qualifierParam = extractQualifierParameter req
   in
     case (typeParam, qualifierParam) of
       (Just type', Just qualifier) ->
         case qualifier of
           Effective -> effAgainst type'
           Weak -> weakA type'

gameLocationWebhookRequest :: WebhookRequest -> PokeApi [String]
gameLocationWebhookRequest req = do
  manager' <- liftIO $ newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager' (BaseUrl Https "pokeapi.co" 443 "/api/v2")
   in case getParams req of
        Just EncounterParams{..} -> pokemonEncounterByGame clientEnv pkmn game
        Nothing -> return $ Right []

getParams :: WebhookRequest -> Maybe EncounterParams
getParams req = do
  game <- extractGameParameter req
  oCtxs <- outputContexts (queryResult req)
  pkmn <- getContextParameter oCtxs (session req <> "/contexts/getpokemonlocation-followup") "Pokemon"
  return EncounterParams{..}

fulFillmentAPI :: Proxy API
fulFillmentAPI = Proxy

server :: Server API
server = fulfillment

-- server :: ClientEnv -> Server API
-- server clientEnv = hoistServer fulFillmentAPI (nat clientEnv) fulfillment
--   where
--     nat :: HasPokeApi m => ClientEnv -> m a -> Handler a
--     nat env x = do
--       a <- runExceptT (runReaderT x env)
--       case a of
--         Left err -> error "whatever"
--         Right r -> return (Right r)

app :: ClientEnv -> Application
app clientEnv = serve fulFillmentAPI (server clientEnv)
