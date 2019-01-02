-- | Start a server serving Progress backend and frontend.
module Main where

import           System.Environment (getArgs)

import           Telescope.Monad    (runScope)

import           Config             (Config (..), getConfig)
import qualified Server
import           Tutorial           (example)

main :: IO ()
main = do
  [configName] <- getArgs
  getConfig configName >>= \case
    Nothing -> putStrLn $ "No Config named '" ++ configName ++ "'"
    Just config -> do
      putStrLn $ "Using: " ++ show config
      putStrLn "Running 'Tutorial.example'"
      runScope (_configSourceConfig config) Tutorial.example
      putStrLn $ "Starting server on port " ++ show (_configPort config)
      Server.run config
