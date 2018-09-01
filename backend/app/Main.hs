-- | Start the Progress server.
module Main where

import qualified Database as Db
import           Server   (run)
import           Tutorial (example)

main :: IO ()
main = do
  let port = 8081
  db <- Db.new []
  putStrLn "Populating database from 'Tutorial.example'"
  Db.run db example
  putStrLn $ "Starting server on port " ++ show port
  run port db
