{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}

-- | High level functions for the frontend.
module App where

import           Data.Default                     (def)
import           Data.Maybe                       (fromJust)
import qualified Language.Javascript.JSaddle.Warp as Warp
import           Reflex.Dom
import qualified Reflex.Dom.Core                  as Core

import qualified Client as C
import           FrontendModel

title :: MonadWidget t m => m ()
title = text "Progress"

-- | The root widget of our frontend.
app :: MonadWidget t m => m ()
app = do
  el "div" title
  display =<< C.firstRegionByNameDyn "World"
  display =<< C.allUsersDyn
  display =<< C.allRegionsDyn

-- | Build the frontend into static files.
reflexMain :: IO ()
reflexMain = mainWidget app

-- | Serve a built frontend with @jsaddle-warp@.
warpMain :: IO ()
warpMain = Warp.run 3003 $ Core.mainWidget app
