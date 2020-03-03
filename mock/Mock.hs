{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Mock
import Data.Text (pack)
import Test.QuickCheck

import Api
import Model

instance Arbitrary SpiceGirl where
    arbitrary = do
      name <- pack <$> arbitrary
      alias <- pack <$> arbitrary
      pk <- arbitrary
      ranking <- arbitrary
      pure SpiceGirl {..}

mockApi :: Proxy GirlsAPI
mockApi = Proxy

main :: IO ()
main = run 8080 $ serve mockApi (mock mockApi Proxy)
