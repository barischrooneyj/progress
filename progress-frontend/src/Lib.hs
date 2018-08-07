{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Language.Javascript.JSaddle.Warp (run)
import           Reflex.Dom                       (el, mainWidget, text)

app = el "div" $ text "Welcome to Reflex"

warp :: IO ()
warp = run 3911 $ mainWidget app
