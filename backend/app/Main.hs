-- | Start the Progress server.
module Main where

import           Data.Map           as Map
import           System.Environment (getArgs)

import           Config
import qualified Database           as Db
import qualified Server
import           Tutorial           (example)

main :: IO ()
main = do
  [configName] <- getArgs
  case Map.lookup configName configMap of
    Nothing -> putStrLn $ "No such config '" ++ configName ++ "'"
    Just config -> do
      putStrLn $ "Using " ++ configName ++ " config"
      db <- Db.new []
      putStrLn "Populating database from 'Tutorial.example'"
      Db.run db example
      putStrLn $ "Starting server on port " ++ show (_configPort config)
      Server.run db config
