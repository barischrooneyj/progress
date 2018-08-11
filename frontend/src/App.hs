{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | The top level functions of the frontend.
module App where

import Data.Default (def)
import qualified Language.Javascript.JSaddle.Warp as Warp
import           Reflex.Dom
import Reflex.Class (tag, constant)
import qualified Reflex.Dom.Core                  as Core

import Lib (User)

title :: MonadWidget t m => m ()
title = text "hi"

-- | The root widget of our frontend.
app :: MonadWidget t m => m ()
app = do
  el "div" $ title
  buttonEvent <- button "click me"
  let request = xhrRequest "GET" "/user/all" def
  asyncEvent <- performRequestAsync (tag (constant request) buttonEvent)
  el "hi" $ title
  -- buttonDyn <- holdDyn (Just $ Response "Nothing yet") $
  --                fmap decodeXhrResponse asyncEvent
  -- display buttonDyn

-- | Build the frontend into static files.
reflexMain :: IO ()
reflexMain = mainWidget app

-- | Serve a built frontend with @jsaddle-warp@.
warpMain :: IO ()
warpMain = Warp.run 3003 $ Core.mainWidget app
