{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE OverloadedStrings   #-}

-- | Clients functions for querying the server.
module Client where

import           Data.Aeson     (FromJSON)
import           Data.Text      (Text)
import qualified Data.Text      as Text
import           Reflex.Dom

import           Config
import           FrontendModel

-- | A list of all 'User'.
allUsersDyn :: MonadWidget t m => m (Dynamic t (Maybe [User]))
allUsersDyn = allDyn User{} "user" devConfig

-- | A list of all 'Region'.
allRegionsDyn :: MonadWidget t m => m (Dynamic t (Maybe [Region]))
allRegionsDyn = allDyn Region{} "region" devConfig

-- | The first 'Region' with name equal to that given.
firstRegionByNameDyn :: MonadWidget t m => Text -> m (Dynamic t (Maybe (Maybe Region)))
firstRegionByNameDyn name = firstWhereDyn Region{} "region" "name" name devConfig

-- | The first value of type 'a' where a field is equal to a given value.
firstWhereDyn :: (FromJSON a, MonadWidget t m) =>
  a -> Text -> Text -> Text -> Config -> m (Dynamic t (Maybe (Maybe a)))
firstWhereDyn a type' field name config = do
  startEvent <- getPostBuild
  let request = postJson (Text.concat ["http://localhost:", Text.pack $ show $ _configStaticPort config, "/", type', "/", field]) name
  asyncEvent <- performRequestAsync $ tag (constant request) startEvent
  holdDyn (Just Nothing) $ fmap (Just . decodeXhrResponse) asyncEvent

-- | A list of all values of type 'a'.
allDyn :: (FromJSON a, MonadWidget t m) =>
  a -> Text -> Config -> m (Dynamic t (Maybe [a]))
allDyn _ type' config = do
  startEvent <- getPostBuild
  let request = xhrRequest "GET"
        (Text.concat ["http://localhost:", Text.pack $ show $ _configStaticPort config, "/", type', "/all"]) def
  asyncEvent <- performRequestAsync $ tag (constant request) startEvent
  holdDyn (Just []) $ fmap decodeXhrResponse asyncEvent
