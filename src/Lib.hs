{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Lib (app) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT, ExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import PokeApi.Pokemon.Queries
import PokeApi.Pokemon.Types
import PokeApi.Types
import PokeApi.Type.Types
import PokeApi.Type.Queries
import Data.List (intercalate)
import Servant.Client (ClientEnv, ClientError)
import Servant

import qualified Data.Map as M
import qualified Data.Text as T

import Types

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

fulfillment :: WebhookRequest -> PokeApi WebhookResponse
fulfillment req = do
  liftIO $ putStrLn "WebhookRequest!"
  case (intent . queryResult) req of
    Nothing -> error "No intent." -- TODO: this should obviously not throw an error.
    Just intent -> fulfillIntent req (displayName intent)

fulfillIntent :: WebhookRequest -> String -> PokeApi WebhookResponse
fulfillIntent req = \case
  "Get types" -> createResponse <$> pokeApiWebhookRequest req
  "Get Pokemon location" ->
    let msg = "In what game?"
        speechResponse = SimpleResponse (TextToSpeech msg) Nothing
        response = G.Response True Nothing (G.RichResponse  [G.Item $ G.SimpleResponse speechResponse] [] Nothing)
        payload = Just $ G.GooglePayload response
     in return $ WebhookResponse (Just msg) (Just [Message $ SimpleResponses [speechResponse]]) (Just "mauriciofierro.dev") payload Nothing Nothing
  "Get Pokemon location - custom" -> createFollowupResponse <$> gameLocationWebhookRequest req

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
       (Just type', Just qualifier) ->
         case qualifier of
           Effective -> effectiveAgainst type'
           Weak -> weakAgainst type'

gameLocationWebhookRequest :: WebhookRequest -> PokeApi [String]
gameLocationWebhookRequest req =
  case getParams req of
    Just EncounterParams{..} -> pokemonEncounterByGame pkmn game
    Nothing -> lift . except $ Right []

getParams :: WebhookRequest -> Maybe EncounterParams
getParams req = do
  game <- extractGameParameter req
  oCtxs <- outputContexts (queryResult req)
  pkmn <- getContextParameter oCtxs (session req <> "/contexts/getpokemonlocation-followup") "Pokemon"
  return EncounterParams{..}

fulFillmentAPI :: Proxy API
fulFillmentAPI = Proxy

nt :: ClientEnv -> PokeApi a -> Handler a
nt env x = do
  sdf <- liftIO . runExceptT $ runReaderT x env
  Handler $ clientToServerError sdf
  where
    clientToServerError :: Either ClientError a -> ExceptT ServerError IO a
    clientToServerError eca =
      case eca of
        Left e -> except . Left $ err300 { errReasonPhrase = show e }
        Right v -> except (Right v)

server :: ServerT API PokeApi
server = fulfillment

mainServ env = hoistServer fulFillmentAPI (nt env) server

app :: ClientEnv -> Application
app env = serve fulFillmentAPI (mainServ env)
