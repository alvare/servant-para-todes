{-# LANGUAGE DuplicateRecordFields #-}

module Server
 ( app
 ) where

import Control.Monad.IO.Class
import Network.Wai (Application)
import Servant
import Data.List   (find)

import Api
import Data
import Model

app :: Application
app = serve api server


api :: Proxy API
api = Proxy


server :: Server API
server = releaseApi :<|> girlsApi
  where
    releaseApi = getReleases :<|> getRelease
    girlsApi = getGirls :<|> getGirl

getReleases :: Handler [Release]
getReleases = do
  releases <- liftIO allReleases
  pure releases


getRelease :: Int -> Handler Release
getRelease id' = do
  releases <- liftIO allReleases
  case find byId releases of
    Nothing -> throwError err404
    Just release -> pure release
  where
    byId = (== id') . (pk :: Release -> Int)


getGirls :: Handler [SpiceGirl]
getGirls = do
  girls <- liftIO allGirls
  pure girls


getGirl :: Int -> Handler SpiceGirl
getGirl id' = do
  girls <- liftIO allGirls
  case find byId girls of
    Nothing -> throwError err404
    Just girl -> pure girl
  where
    byId = (== id') . (pk :: SpiceGirl -> Int)
