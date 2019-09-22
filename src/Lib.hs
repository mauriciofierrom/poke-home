{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib (app) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except, runExceptT, ExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Dialogflow.V2.Fulfillment.Message
import Dialogflow.V2.Fulfillment.Webhook.Request
import Dialogflow.V2.Fulfillment.Webhook.Response hiding (outputContexts)
import PokeApi.Common
import Servant.Client (ClientEnv, ClientError)
import Servant

import qualified Dialogflow.V2.Fulfillment.Payload.Google as G

import TypeIntent
import LocationIntent

type API = "fulfillment" :> ReqBody '[JSON] WebhookRequest :> Post '[JSON] WebhookResponse

fulfillment :: WebhookRequest -> PokeApi WebhookResponse
fulfillment req = do
  liftIO $ putStrLn "WebhookRequest!"
  case (intent . queryResult) req of
    Nothing -> error "No intent." -- TODO: this should obviously not throw an error.
    Just intent -> fulfillIntent req (displayName intent)

fulfillIntent :: WebhookRequest -> String -> PokeApi WebhookResponse
fulfillIntent req = \case
  "Get types" -> createTypeResponse <$> pokeApiWebhookRequest req
  "Get Pokemon location" ->
    let msg = "In what game?"
        speechResponse = SimpleResponse (TextToSpeech msg) Nothing
        response = G.Response True Nothing (G.RichResponse  [G.Item $ G.SimpleResponse speechResponse] [] Nothing)
        payload = Just $ G.GooglePayload response
     in return $ WebhookResponse (Just msg) (Just [Message $ SimpleResponses [speechResponse]]) (Just "mauriciofierro.dev") payload Nothing Nothing
  "Get Pokemon location - custom" -> createFollowupResponse <$> gameLocationWebhookRequest req

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
