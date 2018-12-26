-- | Start a server serving Progress backend and frontend.
module Main where

import           System.Environment (getArgs)

import           Telescope.Class    (runWith)

import           Config             (Config (..), getConfig, _configStoreConfig)
import qualified Server
import qualified Tutorial

main :: IO ()
main = do
  [configName] <- getArgs
  getConfig configName >>= \case
    Nothing -> putStrLn $ "No Config named '" ++ configName ++ "'"
    Just config -> do
      putStrLn $ "Using: " ++ show config
      putStrLn "Populating database from 'Tutorial.example'"
      runWith (_configStoreConfig config) Tutorial.example
      putStrLn $ "Starting server on port " ++ show (_configPort config)
      Server.run config
