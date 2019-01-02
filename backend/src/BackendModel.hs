{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Instances for 'Model' data types specific to the backend.
module BackendModel
  ( module BackendModel
  , module Model
  ) where

import           Control.Lens              hiding (children)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Set                  as Set
import           GHC.Generics              (Generic)
import           Numeric.Units.Dimensional (Dimension' (..))

import           Telescope.Storable        (Storable (..),
                                            PKey (..))

import           Model

deriving instance ToJSON Dimension'

-- * 'User'.

makeLensesWith camelCaseFields ''User

instance PKey User Username where
  pKey u = u ^. username

instance Storable User Username

deriving instance ToJSON User

-- * 'Metric'.

makeLensesWith camelCaseFields ''Metric

instance PKey Metric MetricKey where
  pKey m = (m ^. name, m ^. dimension)

instance Storable Metric MetricKey

deriving instance ToJSON Metric

-- * 'Region'.

makeLensesWith camelCaseFields ''Region

instance PKey Region RegionName where
  pKey r = r ^. name

instance Storable Region RegionName

-- instance Storable Region RegionName where
--   onSetUpdates = const [updateRegionChildren]
--     where updateRegionChildren = Update $ \a -> (
--               Region{}
--             , Set.toList $ a ^. parents
--             , \(b :: Region) -> b & children %~ Set.insert (a ^. name)
--             )

deriving instance ToJSON Region

-- * 'Progress'.

makeLensesWith camelCaseFields ''Progress

instance PKey Progress ProgressKey where
  pKey p = (p ^. metric, p ^.region)

instance Storable Progress ProgressKey where
--   onSetUpdates = const [updateRegionProgress]
--     where updateRegionProgress = Update $ \a -> (
--               Region{}
--             , [a ^. region]
--             , \(b :: Region) -> b & progress %~ Set.insert (a ^. ident)
--             )

deriving instance ToJSON Progress

-- * 'Target'.

makeLensesWith camelCaseFields ''Target

deriving instance ToJSON Target

-- * 'Targets'.

makeLensesWith camelCaseFields ''Targets

instance PKey Targets TargetsKey where
  pKey t = (t ^. metric, t ^.region)

instance Storable Targets TargetsKey

deriving instance ToJSON Targets

-- * 'Rep'.

makeLensesWith camelCaseFields ''Rep

instance PKey Rep RepKey where
  pKey r = (r ^. name, r ^.region)

instance Storable Rep RepKey

deriving instance ToJSON Rep
