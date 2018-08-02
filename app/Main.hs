module Main where

import qualified Database as Db
import           Server   (run)
import           Tutorial (example)

main :: IO ()
main = do
  let port = 8081
  db <- Db.newStore
  Db.run db example
  putStrLn "Populated database"
  putStrLn $ "Starting server on port " ++ show port
  run port db
