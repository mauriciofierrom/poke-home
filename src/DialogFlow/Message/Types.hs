{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module DialogFlow.Message.Types
  ( Button(..)
  , MText(..)
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
    "text" .= object [
    "text" .= tText t ]
                    ]

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
  SimpleResponse { textToSpeech :: String -- these fields are mutually exclusive vv
                 , ssml :: String -- ^^ these fields are mutually exclusive ^^
                 , displayText :: Maybe String
                 } deriving (Eq, Generic, Show)

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
