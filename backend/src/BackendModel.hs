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

import           Control.Lens
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Set                  as Set
import           Database.Store.Class      (Consistent, Identifiable, Storable,
                                            Update (..), key, onAdd)
import           GHC.Generics              (Generic)
import           Numeric.Units.Dimensional (Dimension' (..))

import           Model

deriving instance ToJSON Dimension'

-- * 'User'.

makeLensesWith camelCaseFields ''User

instance Identifiable User Username where
  key u = u ^. username
instance Storable User Username
instance Consistent User Username

deriving instance Generic User
deriving instance ToJSON User

-- * 'Metric'.

makeLensesWith camelCaseFields ''Metric

instance Identifiable Metric MetricKey where
  key m = (m ^. name, m ^. dimension)
instance Storable Metric MetricKey
instance Consistent Metric MetricKey

deriving instance Generic Metric
deriving instance ToJSON Metric

-- * 'Region'.

makeLensesWith camelCaseFields ''Region

instance Identifiable Region RegionName where
  key r = r ^. name
instance Storable Region RegionName where
instance Consistent Region RegionName where
  -- onAdd = [updateProgress]
  --   where updateProgress = Update (a ->
  --           ())

deriving instance Generic Region
deriving instance ToJSON Region

-- * 'Progress'.

makeLensesWith camelCaseFields ''Progress

instance Identifiable Progress ProgressKey where
  key p = (p ^. metric, p ^.region)
instance Storable Progress ProgressKey
instance Consistent Progress ProgressKey where
  onAdd = const [updateRegion]
    -- Add a reference to a 'Region' about this 'Progress'.
    where updateRegion = Update $ \a -> (
              [a ^. region],
              \(b :: Region) -> b & progress %~ Set.insert (a ^. ident)
            )

deriving instance Generic Progress
deriving instance ToJSON Progress

-- * 'Target'.

makeLensesWith camelCaseFields ''Target

deriving instance Generic Target
deriving instance ToJSON Target

-- * 'Targets'.

makeLensesWith camelCaseFields ''Targets

instance Identifiable Targets TargetsKey where
  key t = (t ^. metric, t ^.region)
instance Storable Targets TargetsKey
instance Consistent Targets TargetsKey

deriving instance Generic Targets
deriving instance ToJSON Targets

-- * 'Rep'.

makeLensesWith camelCaseFields ''Rep
instance Identifiable Rep RepKey where
  key r = (r ^. name, r ^.region)
instance Storable Rep RepKey
instance Consistent Rep RepKey

deriving instance Generic Rep
deriving instance ToJSON Rep

