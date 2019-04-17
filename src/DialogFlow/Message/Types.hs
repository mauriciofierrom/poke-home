{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module DialogFlow.Message.Types
  ( Button(..)
  , MText(..)
  , MSimpleResponse(..)
  , SimpleResponse(..)
  ) where

import Data.Aeson (FromJSON, parseJSON, ToJSON, toJSON, object, withObject, (.:), (.=))
import Data.Maybe (catMaybes)
import GHC.Generics

newtype MText = MText { tText :: Maybe [String] } deriving (Eq, Show)

instance FromJSON MText where
  parseJSON = withObject "text" $ \o -> do
    firstText <- o .: "text"
    tText <- firstText .: "text"
    return MText{..}

instance ToJSON MText where
  toJSON t = object [
    -- "text" .= object [
    "text" .= tText t ]
                    -- ]

newtype MSimpleResponse = MSimpleResponse { mTextToSpeech :: String } deriving (Eq, Show)

instance FromJSON MSimpleResponse where
  parseJSON = withObject "simpleResponses" $ \sr ->
    MSimpleResponse <$> sr .: "textToSpeech"

instance ToJSON MSimpleResponse where
  toJSON sr =
    -- object [ "simpleResponses" .=
      object [ "textToSpeech" .= mTextToSpeech sr
             ]
           -- ]

data Image = Image { iImageUri :: Maybe String
                     , accessibilityText :: Maybe String
                     } deriving (Eq, Generic, Show)

data QuickReply = QuickReply { title :: Maybe String
                             , quickReplies :: Maybe [String]
                             } deriving (Eq, Generic, Show)

data Button = Button { buttonText :: Maybe String
                     , buttonPostback :: Maybe String
                     } deriving (Eq, Generic, Show)

data Card = Card { cCardTitle :: Maybe String
                 , cSubtitle :: Maybe String
                 , cImageUri :: Maybe String
                 , cButtons :: Maybe [Button]
                 } deriving (Eq, Generic, Show)

data SimpleResponse =
  SimpleResponse { textToSpeech :: Maybe String -- these fields are mutually exclusive vv
                 -- , ssml :: Maybe String -- ^^ these fields are mutually exclusive ^^
                 -- , displayText :: Maybe String
                 } deriving (Eq, Show)

instance FromJSON SimpleResponse where
  parseJSON = withObject "simpleResponse" $ \sr -> do
    simpleResponse <- sr .: "simpleResponse"
    textToSpeech <- simpleResponse .: "textToSpeech"
    -- ssml <- simpleResponse .: "ssml"
    -- displayText <- simpleResponse .: "displayText"
    return SimpleResponse{..}

instance ToJSON SimpleResponse where
  toJSON sr =
    object [ "simpleResponse" .=
      object [ "textToSpeech" .= textToSpeech sr ]
             -- , "ssml" .= ssml sr
             -- , "displayText" .= displayText sr ]
           ]

data BasicCard =
  BasicCard { bsTitle :: Maybe String
            , bsSubtitle :: Maybe String
            , bsFormattedText :: String
            , bsImage :: Maybe Image
            , bsButtons :: [Button]
            } deriving (Eq, Generic, Show)

newtype Suggestions =
  Suggestions { sugTitle :: String } deriving (Eq, Generic, Show)

data LinkOutSuggestion =
  LinkOutSuggestion { losDestinationName :: String
                    , losUri :: String
                    } deriving (Eq, Generic, Show)

data SelectItemInfo =
  SelectItemInfo { siiKey :: String
                 , siiSynonyms :: Maybe [String] } deriving (Eq, Generic, Show)

data Item =
  Item { iInfo :: SelectItemInfo
       , iTitle :: String
       , iDescription :: Maybe String
       , iImage :: Maybe Image
       } deriving (Eq, Generic, Show)

data ListSelect =
  ListSelect { lsTitle :: Maybe String
             , lsItems :: [Item]
             } deriving (Eq, Generic, Show)

newtype CarouselSelect =
  CarouselSelect { csItems :: [Item] } deriving (Eq, Generic, Show)
