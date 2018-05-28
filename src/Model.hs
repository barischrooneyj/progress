{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE IncoherentInstances    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Model where

-- * Modeling the progress and targets of regions.
--
-- | From reading through the data types and their equality instances you will
-- notice that the 'Region' is a core data type. Everything is connected to a
-- region and they can be connected to each other.

-- ** TODO: Handle error cases in 'add'.
-- ** TODO: Add lookup methods.
-- ** TODO: Add physical database, perhaps via Groundhog.
-- ** TODO: Add HTTP API.

-- ** TODO: Suggest metrics for region via sibling regions.
-- ** TODO: Get progress of a region in terms of children.
-- ** TODO: Get progress of other regions on same metric.

import           Control.Lens
import qualified Data.DateTime             as T
import           Data.Map                  (Map)
import           Data.Ord
import           Data.Set                  (Set)
import           Numeric.Units.Dimensional (Dimension' (..))

-- ** The data types.

-- | First the numerous type aliases!
type DateTime      = T.DateTime
type Dimension     = Dimension'
type DimensionName = String
type Email         = String
type ID            = Integer
type Measurement   = (MetricValue, DateTime)
type MetricId      = ID
type MetricName    = String
type MetricSymbol  = String
type MetricValue   = Double
type PasswordHash  = String
type Phone         = String
type ProgressId    = ID
type RegionId      = ID
type RegionName    = String
type RepId         = ID
type RepName       = String
type TargetDesc    = String
type TargetsId     = ID
type Username      = String

deriving instance Read Dimension'

-- | A user of the website.
data User = User {
    _userUsername :: Username
  , _userHome     :: Maybe RegionName
  , _userPwdHash  :: PasswordHash
  } deriving (Read, Show)
makeLensesWith camelCaseFields ''User

-- | Users are compared only by username.
instance Eq User where
  a == b = a <= b && a >= b
instance Ord User where
  compare = comparing _userUsername

-- | A measurable quantity like "CO2 emissions" or "Plastic tax".
data Metric = Metric {
    _metricIdent :: MetricId
  , _metricOwner :: Username
  , _metricName  :: MetricName
  , _metricDim   :: Either Dimension MetricSymbol
  } deriving (Read, Show)
makeLensesWith camelCaseFields ''Metric

-- | Metrics are compared by names and dimensions.
-- instance Eq Metric where
--   a == b = a <= b && a >= b
-- instance Ord Metric where
--   compare = comparing (\m -> (m ^. name, m ^.dim))

-- | Progress and targets for multiple metrics, under one region.
data Region = Region {
    _regionIdent    :: RegionId
  , _regionOwner    :: Username
  , _regionName     :: RegionName
  , _regionProgress :: Set ProgressId
  , _regionTargets  :: Set TargetsId
  , _regionReps     :: [RepId] -- ^ Order of importance.
  , _regionParents  :: Set RegionName
  , _regionChildren :: Set RegionName
  } deriving (Read, Show)
makeLensesWith camelCaseFields ''Region

-- | Measurements for one (metric, region).
data Progress = Progress {
    _progressIdent  :: ProgressId
  , _progressOwner  :: Username
  , _progressMetric :: MetricId
  , _progressRegion :: RegionName
  , _progressReps   :: [RepId]
  , _progressValues :: [Measurement]
  } deriving (Read, Show)
makeLensesWith camelCaseFields ''Progress

-- | Progress are compared by (metric, region).
instance Eq Progress where
  a == b = a <= b && a >= b
instance Ord Progress where
  compare = comparing (\p -> (p ^. metric, p ^.region))

-- | A target for some metric with a description.
data Target = Target {
    _targetIncrease    :: Bool
  , _targetValue       :: MetricValue
  , _targetDescription :: TargetDesc
  , _targetDate        :: DateTime
  -- | Compared by all fields.
  } deriving (Eq, Ord, Read, Show)
makeLensesWith camelCaseFields ''Target

-- | Targets for one (metric, region).
data Targets = Targets {
    _targetsIdent  :: TargetsId
  , _targetsOwner  :: Username
  , _targetsMetric :: MetricId
  , _targetsRegion :: RegionName
  , _targetsValues :: [Target]
  } deriving (Read, Show)
makeLensesWith camelCaseFields ''Targets

-- | Targets are compared by (metric, region).
instance Eq Targets where
  a == b = a <= b && a >= b
instance Ord Targets where
  compare = comparing (\t -> (t ^. metric, t ^.region))

-- | A contactable representative.
data Rep = Rep {
    _repIdent  :: RepId
  , _repOwner  :: Username
  , _repName   :: RepName
  , _repRegion :: RegionName
  , _repEmail  :: [Email]
  , _repPhone  :: [Phone]
  } deriving Show
makeLensesWith camelCaseFields ''Rep

-- | Representatives are compared by (name, region).
instance Eq Rep where
  a == b = a <= b && a >= b
instance Ord Rep where
  compare = comparing (\r -> (r ^. name, r ^.region))

-- | Each field is like a table in a database.
data Database = Database {
    _databaseIdent    :: ID
  , _databaseUsers    :: Map Username User
  , _databaseMetrics  :: Map MetricId Metric
  , _databaseRegions  :: Map RegionName Region
  , _databaseProgress :: Map ProgressId Progress
  , _databaseTargets  :: Map TargetsId Targets
  , _databaseReps     :: Map RepId Rep
  } deriving Show
makeLensesWith camelCaseFields ''Database
