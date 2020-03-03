{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Server
 ( app
 , api
 ) where

import Control.Monad.IO.Class
import Network.Wai (Application)
import Servant
import Data.Ord    (Down(..))
import Data.List   (sortOn, sort, find)

import Api
import Data
import Model

app :: Application
app = serve api server


api :: Proxy API
api = Proxy


server :: Server API
server = releaseApi :<|> spicesApi
  where
    releaseApi = getReleases :<|> getRelease
    spicesApi = getSpices :<|> getSpice :<|> addSpice


getReleases :: Maybe Sort -> Handler [Release]
getReleases maybeSort = do
  releases <- liftIO allReleases
  pure $ sortReleases releases
 where
  sortReleases releases = case maybeSort of
    Nothing -> releases
    Just sort' -> case sort' of
      ASC -> sortOn country releases
      DES -> sortOn (Down . country) releases

getRelease :: Int -> Handler Release
getRelease id' = do
  releases <- liftIO allReleases
  case find byId releases of
    Nothing -> throwError err404
    Just release -> pure release
 where
  byId = (== id') . (pk :: Release -> Int)


addSpice :: NewSpiceGirl -> Handler SpiceGirl
addSpice NewSpiceGirl {..} = do
  spices <- liftIO allSpices
  let pk = newestPk spices + 1
  let newSpice = SpiceGirl {..}
  let spices' = spices <> [newSpice]
  liftIO $ saveSpices spices'
  pure newSpice
 where
  newestPk = head . reverse . sort . map (pk :: SpiceGirl -> Int)


getSpices :: Handler [SpiceGirl]
getSpices = do
  spices <- liftIO allSpices
  pure spices


getSpice :: Int -> Handler SpiceGirl
getSpice id' = do
  spices <- liftIO allSpices
  case find byId spices of
    Nothing -> throwError err404
    Just spice -> pure spice
 where
  byId = (== id') . (pk :: SpiceGirl -> Int)
