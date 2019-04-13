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
                -- , fulfillmentMessages :: M.Map String String
                , outputContexts :: Maybe DFOutputContext
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
             , dfrFulfillmentMessages :: Maybe [DF.MText] -- check how to use an unified data type
             , dfrSource :: Maybe String
             -- , dfrPayload :: M.Map String String
             } deriving (Eq, Show)

instance FromJSON DFResponse where
  parseJSON = withObject "response" $ \d -> do
    dfrFulfillmentText <- d .: "fulfillmentText"
    dfrFulfillmentMessages <- d .: "fulfillmentMessages"
    dfrSource <- d .: "source"
    -- dfrPayload <- d .: "payload"
    return DFResponse{..}

instance ToJSON DFResponse where
  toJSON d = object [
    "fulfillmentText" .= dfrFulfillmentText d,
    "fulfillmentMessages" .= dfrFulfillmentMessages d,
    "source" .= dfrSource d
                    ]

data Qualifier = Weak | Effective deriving (Eq, Show)
