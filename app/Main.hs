module Main where

import           Database.Store.Tutorial (example, run)
import           Tutorial                (runExample)

main :: IO ()
main = do
  -- | Tutorial code.
  runExample
  -- | Database experiment.
  print =<< run example
