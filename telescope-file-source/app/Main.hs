{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import           Control.Concurrent     (threadDelay)
import           Data.ByteString.Char8  (unpack)
import           Text.Pretty.Simple     (pPrint)

import           Telescope.Operations   (onChangeK, set, viewAll)
import           Telescope.Monad        (runScope)
import           Telescope.Storable     (toSDataType)
import           Telescope.Source.File  (fileConfig)
import           Telescope.TutorialCode (Other (..), User (..), example)

main :: IO ()
main = do
  config <- fileConfig "dbs/app-main"
  runScope config example
  let other = Other 1 "hi" "there"
  runScope config $ set $ User "Foo" "1234" Nothing other
  runScope config $ set $ User "Bar" "1234" Nothing other
  runScope config $ onChangeK User{} "Angela" $
    \v -> putStrLn $ "Handler for Angela: " ++ unpack v
  runScope config $ onChangeK User{} "Foo" $
    \v -> putStrLn $ "Handler for Foo: " ++ unpack v
  runScope config $ set $ User "Foo"    "5678" Nothing other
  runScope config $ set $ User "Carl"   "5678" Nothing other
  runScope config $ set $ User "Angela" "5678" Nothing other
  threadDelay $ 100 * 1000  -- 100 milliseconds
  pPrint $ toSDataType $ User "Foo" "1111" Nothing other
  pPrint =<< runScope config (viewAll User{})
