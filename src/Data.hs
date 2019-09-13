{-# LANGUAGE OverloadedStrings #-}

module Data where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import           Data.Csv
import           Data.Vector (toList)

import Model


instance FromField Format where
  parseField = pure . read . BS.unpack

instance ToField Format where
  toField = BS.pack . show

instance FromNamedRecord SpiceGirl
instance ToNamedRecord SpiceGirl
instance DefaultOrdered SpiceGirl

instance FromNamedRecord Release
instance ToNamedRecord Release
instance DefaultOrdered Release

allGirls :: IO [SpiceGirl]
allGirls = getAll "girls"


saveGirls :: [SpiceGirl] -> IO ()
saveGirls = saveAll "girls"


allReleases :: IO [Release]
allReleases = getAll "releases"


saveReleases :: [Release] -> IO ()
saveReleases = saveAll "releases"


getAll :: FromNamedRecord a => String -> IO [a]
getAll name = do
  csvData <- BL.readFile $ "data/" <> name <> ".csv"
  case decodeByName csvData of
    Left err -> error err
    Right (_, v) -> pure (toList v)


saveAll :: (DefaultOrdered a, ToNamedRecord a) => String -> [a] -> IO ()
saveAll name els = do
  let csvData = encodeDefaultOrderedByName els
  BL.writeFile ("data/" <> name <> ".csv") csvData
