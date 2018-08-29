{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE OverloadedStrings   #-}

-- | Clients functions for querying the server.
module Client where

import           Data.Aeson     (FromJSON)
import           Data.Text      (Text)
import qualified Data.Text      as Text
import           Reflex.Dom

import           FrontendModel

-- | A list of all 'User'.
allUsersDyn :: MonadWidget t m => m (Dynamic t (Maybe [User]))
allUsersDyn = allDyn User{} "user"

-- | A list of all 'Region'.
allRegionsDyn :: MonadWidget t m => m (Dynamic t (Maybe [Region]))
allRegionsDyn = allDyn Region{} "region"

-- | The first 'Region' with name equal to that given.
firstRegionByNameDyn :: MonadWidget t m => Text -> m (Dynamic t (Maybe (Maybe Region)))
firstRegionByNameDyn name = firstWhereDyn Region{} "region" "name" name

-- | The first value of type 'a' where a field is equal to a given value.
firstWhereDyn :: (FromJSON a, MonadWidget t m) =>
  a -> Text -> Text -> Text -> m (Dynamic t (Maybe (Maybe a)))
firstWhereDyn a type' field name = do
  startEvent <- getPostBuild
  let request = postJson (Text.concat ["/", type', "/", field]) name
  asyncEvent <- performRequestAsync $ tag (constant request) startEvent
  holdDyn (Just Nothing) $ fmap (Just . decodeXhrResponse) asyncEvent

-- | A list of all values of type 'a'.
allDyn :: (FromJSON a, MonadWidget t m) =>
  a -> Text -> m (Dynamic t (Maybe [a]))
allDyn _ type' = do
  startEvent <- getPostBuild
  let request = xhrRequest "GET" (Text.concat ["/", type', "/all"]) def
  asyncEvent <- performRequestAsync $ tag (constant request) startEvent
  holdDyn (Just []) $ fmap decodeXhrResponse asyncEvent
