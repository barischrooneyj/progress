{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | The top level functions of the frontend.
module App where

import           Data.Default                     (def)
import qualified Language.Javascript.JSaddle.Warp as Warp
import           Reflex.Class                     (constant, tag)
import           Reflex.Dom
import qualified Reflex.Dom.Core                  as Core

import           FrontendModel                    (User (..))

title :: MonadWidget t m => m ()
title = text "hi"

-- | The root widget of our frontend.
app :: MonadWidget t m => m ()
app = do
  el "div" $ text "steven"
  el "div" title
  buttonEvent <- button "click me"
  let request = xhrRequest "GET" "/user/all" def
  asyncEvent <- performRequestAsync (tag (constant request) buttonEvent)
  el "div" title
  buttonDyn <- holdDyn (Just $ [User "NoUsername" (Just "NoRegion") "NoPassword"]) $
                 fmap decodeXhrResponse asyncEvent
  display buttonDyn

-- | Build the frontend into static files.
reflexMain :: IO ()
reflexMain = mainWidget app

-- | Serve a built frontend with @jsaddle-warp@.
warpMain :: IO ()
warpMain = Warp.run 3003 $ Core.mainWidget app
