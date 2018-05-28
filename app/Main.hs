module Main where

import           DatabaseClass (run)
import           Tutorial      (runExample)

main :: IO ()
main = do
  runExample
  print =<< run
