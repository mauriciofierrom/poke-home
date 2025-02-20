{-# LANGUAGE OverloadedStrings #-}

module TypeIntent where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.Text as T

import PokeApi.Common
import PokeApi.Type
import Dialogflow.V2.Fulfillment.Webhook.Response
import Dialogflow.V2.Fulfillment.Webhook.Request

import Dialogflow.V2.Fulfillment.Message
import qualified Dialogflow.V2.Fulfillment.Payload.Google as G

data Qualifier = Weak | Effective deriving (Eq, Show)

extractTypeParameter ::  WebhookRequest -> Maybe Type'
extractTypeParameter req = do
    typeParam <- (M.lookup "PokemonType" . parameters . queryResult) req
    getType (T.pack typeParam)

createTypeResponse :: [Type'] -> WebhookResponse
createTypeResponse types =
  let types' = fmap getTypeName types
      msg = T.unpack $ T.intercalate " and " types'
      speechResponse = SimpleResponse (TextToSpeech msg) (Just msg)
      response = G.Response False Nothing (G.RichResponse [G.Item $ G.SimpleResponse speechResponse] [] Nothing)
      payload = Just $ G.GooglePayload response
  in WebhookResponse (Just msg)
                      (Just [Message $ SimpleResponses [speechResponse]])
                      (Just "mauriciofierro.dev")
                      payload
                      Nothing
                      Nothing

typeWebhookRequest :: WebhookRequest -> PokeApi [Type']
typeWebhookRequest req = do
  liftIO $ print req
  let typeParam = extractTypeParameter req
      qualifierParam = extractQualifierParameter req
  case (typeParam, qualifierParam) of
    (Just type', Just qualifier) ->
      case qualifier of
        Effective -> effectiveAgainst type'
        Weak -> weakAgainst type'
    _ -> error "Failed!"

extractQualifierParameter :: WebhookRequest -> Maybe Qualifier
extractQualifierParameter  req = do
    typeParam <- (M.lookup "Quality" . parameters . queryResult) req
    getQualifier typeParam
    where
        getQualifier :: String -> Maybe Qualifier
        getQualifier "weak" = Just Weak
        getQualifier "effective" = Just Effective
        getQualifier _ = Nothing
