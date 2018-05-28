module Main where

import           DatabaseClass (example, run)
import           Tutorial      (runExample)

main :: IO ()
main = do
  -- | Tutorial code.
  runExample
  -- | Database experiment.
  print =<< run example
