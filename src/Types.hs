{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where
import Data.Aeson (FromJSON, parseJSON, ToJSON, toJSON, object, withObject, (.:), (.=))
import GHC.Generics

import qualified Data.Map as M

import qualified DialogFlow.Message.Types as DF

data DFIntent =
  DFIntent { intentName :: String
           , displayName :: String
           } deriving (Eq, Generic, Show)

instance FromJSON DFIntent where
  parseJSON = withObject "intent" $ \i -> do
    intentName <- i .: "name"
    displayName <- i .: "displayName"
    return DFIntent {..}

data DFOutputContext =
  DFOutputContext { name :: String
                  , lifespanCount :: Int
                  , outputContextparameters :: M.Map String String
                  }
                  deriving (Eq, Generic, Show)

instance FromJSON DFOutputContext

data DFQueryResult =
  DFQueryResult { queryText :: String
                , parameters :: M.Map String String
                , allRequiredParamsPresent :: Bool
                , fulfillmentText :: Maybe String
                -- disabled for now, as it doesn't seem necessary for the
                -- current implementation
                -- , fulfillmentMessages :: [FulfillmentMessage]
                -- , outputContexts :: Maybe DFOutputContext
                , intent :: Maybe DFIntent
                , intentDetectionConfidence :: Maybe Float
                -- , diagnosticInfo :: Maybe (M.Map String String)
                , languageCode :: String -- Sum type
                } deriving (Eq, Generic, Show)

instance FromJSON DFQueryResult

data DFRequest =
  DFRequest { responseId :: String
            , session :: String
            , queryResult :: DFQueryResult
            -- , originalDetectIntentRequest :: M.Map String String -- Some kind of object
            } deriving(Eq, Generic, Show)

instance FromJSON DFRequest

{- Doesn't use the following parameters:
   - outputContexts[]
   - followupEventInput[]
-}
data DFResponse =
  DFResponse { dfrFulfillmentText :: Maybe String
             , dfrFulfillmentMessages :: [FulfillmentMessage]
             -- , dfrFulfillmentMessages :: Maybe [DF.MText] -- check how to use an unified data type
             , dfrSource :: Maybe String
             , dfrPayload :: GooglePayload
             } deriving (Eq, Show)

instance FromJSON DFResponse where
  parseJSON = withObject "response" $ \d -> do
    dfrFulfillmentText <- d .: "fulfillmentText"
    dfrFulfillmentMessages <- d .: "fulfillmentMessages"
    dfrSource <- d .: "source"
    dfrPayload <- d .: "payload"
    return DFResponse{..}

instance ToJSON DFResponse where
  toJSON d = object [
    "fulfillmentText" .= dfrFulfillmentText d,
    "fulfillmentMessages" .= dfrFulfillmentMessages d,
    "source" .= dfrSource d,
    "payload" .= dfrPayload d
                    ]

data Qualifier = Weak | Effective deriving (Eq, Show)

data GooglePayload = GooglePayload { expectUserResponse :: Bool
                                   , richResponse :: [DF.SimpleResponse]
                                   } deriving (Eq, Show)

instance FromJSON GooglePayload where
  parseJSON = withObject "payload" $ \gp -> do
    payload <- gp .: "payload"
    googlePayload <- payload .: "google"
    expectUserResponse <- googlePayload .: "expectUserResponse"
    richResponses <- googlePayload .: "richResponse"
    richResponse <- richResponses .: "items"
    return GooglePayload{..}

instance ToJSON GooglePayload where
  toJSON gp =
    object [ "google" .=
      object [ "expectUserResponse" .= expectUserResponse gp
             , "richResponse" .=
               object [ "items" .= richResponse gp ]
             ]
           ]

newtype FulfillmentMessage = FulfillmentMessage { mSimpleResponses :: DF.MSimpleResponses }
  deriving (Eq, Generic, Show)

instance FromJSON FulfillmentMessage
instance ToJSON FulfillmentMessage where
  toJSON s = object [ "simpleResponses" .= mSimpleResponses s ]
