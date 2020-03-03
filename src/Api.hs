{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api  where

import Data.Aeson   (FromJSON, ToJSON)
import Servant.API
import Data.Text    (Text)
import GHC.Generics (Generic)

import Model

type SpiceAPI = ReleaseAPI
           :<|> GirlsAPI

type ReleaseAPI = "releases" :> QueryParam "sort" Sort :> Get '[JSON] [Release]
             :<|> "releases" :> Capture "id" Int :> Get '[JSON] Release

type GirlsAPI = "girls" :> Get '[JSON] [SpiceGirl]
           :<|> "girls" :> Capture "id" Int :> Get '[JSON] SpiceGirl
           :<|> "girls" :> ReqBody '[JSON] NewSpiceGirl :> Post '[JSON] SpiceGirl

type API = "api" :> SpiceAPI


data NewSpiceGirl = NewSpiceGirl
  { name    :: Text
  , alias   :: Text
  , ranking :: Int
  } deriving (Generic)

instance FromJSON NewSpiceGirl


data Sort = ASC | DES deriving (Generic)

instance FromHttpApiData Sort where
  parseQueryParam "asc" = pure ASC
  parseQueryParam "des" = pure DES
  parseQueryParam _     = Left "Unknown sort order."
