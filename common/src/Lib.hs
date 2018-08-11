module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data User = User { username :: String, poop :: String }
