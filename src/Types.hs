{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import GHC.Generics

import qualified Data.Map as M

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

data Qualifier = Weak | Effective deriving (Eq, Show)

