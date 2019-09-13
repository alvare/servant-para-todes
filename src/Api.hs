{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api  where

import Servant.API

import Model

type SpiceAPI = ReleaseAPI
           :<|> GirlsAPI

type ReleaseAPI = "releases" :> Get '[JSON] [Release]
             :<|> "releases" :> Capture "id" Int :> Get '[JSON] Release

type GirlsAPI = "girls" :> Get '[JSON] [SpiceGirl]
           :<|> "girls" :> Capture "id" Int :> Get '[JSON] SpiceGirl

type API = "api" :> SpiceAPI
