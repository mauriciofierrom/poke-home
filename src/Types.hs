{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Aeson (FromJSON)
import Data.HashMap.Lazy (HashMap)
import GHC.Generics

data DFIntent =
  DFIntent { intentName :: String
           , displayName :: String
           } deriving (Eq, Generic, Show)

instance FromJSON DFIntent

data DFOutputContext =
  DFOutputContext { name :: String
                  , lifespanCount :: Int
                  , outputContextparameters :: HashMap String String
                  }
                  deriving (Eq, Generic, Show)

instance FromJSON DFOutputContext

data DFQueryResult =
  DFQueryResult { queryText :: String
                , parameters :: HashMap String String
                , allRequiredParamsPresent :: Bool
                , fulfillmentText :: String
                , fulfillmentMessages :: HashMap String String
                , outputContexts :: DFOutputContext
                , intent :: DFIntent
                , intentDetectionConfidence :: Int
                , diagnosticInfo :: String
                , languageCode :: String -- Sum type
                } deriving (Eq, Generic, Show)

instance FromJSON DFQueryResult

data DFRequest =
  DFRequest { responseId :: String
            , session :: String
            , queryResult :: DFQueryResult
            , originalDetectIntentRequest :: String -- Some kind of object
            } deriving(Eq, Generic, Show)

instance FromJSON DFRequest

data DFResponse
