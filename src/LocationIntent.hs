{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LocationIntent where


import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except)
import Dialogflow.V2.Fulfillment.Message
import Dialogflow.V2.Fulfillment.Webhook.Response hiding (outputContexts)
import Dialogflow.V2.Fulfillment.Webhook.Request
import Data.List (intercalate)
import PokeApi.Pokemon
import PokeApi.Common

import qualified Data.Map as M
import qualified Dialogflow.V2.Fulfillment.Payload.Google as G

extractGameParameter :: WebhookRequest -> Maybe String
extractGameParameter = M.lookup "PokemonGameVersion" . parameters . queryResult

extractEncounterParams :: WebhookRequest -> Maybe EncounterParams
extractEncounterParams req = do
  game <- extractGameParameter req
  oCtxs <- outputContexts (queryResult req)
  name <- getContextParameter oCtxs (session req <> "/contexts/getpokemonlocation-followup") "Pokemon"
  return EncounterParams{..}

gameLocationWebhookRequest :: WebhookRequest -> PokeApi [String]
gameLocationWebhookRequest req =
  case extractEncounterParams req of
    Just EncounterParams{..} -> pokemonEncounterByGame name game
    Nothing -> lift . except $ Right []

createFollowupResponse :: [String] -> WebhookResponse
createFollowupResponse encounters =
  let msg = intercalate " and " encounters
      speechResponse = SimpleResponse (TextToSpeech msg) Nothing
      response = G.Response False Nothing (G.RichResponse [G.Item $ G.SimpleResponse speechResponse] [] Nothing)
      payload = Just $ G.GooglePayload response
   in WebhookResponse (Just msg)
                      (Just [Message $ SimpleResponses [speechResponse]])
                      (Just "mauriciofierro.dev")
                      payload
                      Nothing
                      Nothing