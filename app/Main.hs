module Main where

import Network.Wai.Handler.Warp (run)

import Server

main :: IO ()
main = run 9001 app
