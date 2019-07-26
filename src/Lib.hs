{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( serve
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics
import           Servant.API

data SpiceGirl = SpiceGirl
  { name    :: Text
  , alias   :: Text
  , ranking :: Int
  } deriving (Show, Generic)

instance FromJSON SpiceGirl

instance ToJSON SpiceGirl

data Release = Release
  { title  :: Text
  , year   :: Int
  , cat    :: Text
  , format :: Format
  , tracks :: [Text]
  } deriving (Show, Generic)

instance FromJSON Release

instance ToJSON Release

data Format
  = CD
  | LP
  | Casette
  deriving (Show)

instance FromJSON Format

instance ToJSON Format

type SpiceAPI = ReleaseAPI
           :<|> GirlsAPI

type API = "api" :> SpiceAPI

serve = putStrLn "someFunc"
