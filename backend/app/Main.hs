-- | Start a server serving Progress backend and frontend.
module Main where

import           System.Environment (getArgs)

import           Telescope.Class    (runWith)

import           Config             (getConfig, _configStoreConfig)
import qualified Server
import qualified Tutorial

main :: IO ()
main = do
  [configName] <- getArgs
  case getConfig configName of
    Nothing -> putStrLn $ "No Config named '" ++ configName ++ "'"
    Just config -> do
      putStrLn $ "Using Config: " ++ show config
      putStrLn "Populating database from 'Tutorial.example'"
      runWith (_configStoreConfig config) Tutorial.example
      putStrLn $ "Starting server on port " ++ show (_configPort config)
      Server.runStoreServer config
