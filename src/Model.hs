{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data SpiceGirl = SpiceGirl
  { pk      :: Int
  , name    :: Text
  , alias   :: Text
  , ranking :: Int
  } deriving (Show, Generic)

instance FromJSON SpiceGirl
instance ToJSON SpiceGirl

data Release = Release
  { pk      :: Int
  , title   :: Text
  , year    :: Int
  , label   :: Text
  , cat     :: Text
  , country :: Text
  , format  :: Format
  } deriving (Show, Generic)

instance FromJSON Release
instance ToJSON Release

data Format
  = CD
  | LP
  | Cassette
  | VCD
  deriving (Read, Show, Generic)

instance FromJSON Format
instance ToJSON Format
