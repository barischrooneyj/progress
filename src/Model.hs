{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Model where

-- * Modeling the progress and targets of regions.

-- ** TODO: Suggest metrics for region via sibling regions.
-- ** TODO: Get progress of a region in terms of children.
-- ** TODO: Get progress of other regions on same metric.
-- ** TODO: Better date input format.
-- ** TODO: Correct Eq and Ord instances.
-- ** TODO: Handle error cases in 'add'.

import           Control.Lens              (camelCaseFields, makeLensesWith)
import           Data.Map                  (Map)
import           Data.Set                  (Set)
import           Numeric.Units.Dimensional as Dim

-- ** The data types.

type Dimension     = Dimension'
type DimensionName = String
type Email         = String
type ID            = Integer
type Measurement   = (MetricValue, Seconds)
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
type Seconds       = Integer
type TargetDesc    = String
type TargetsId     = ID
type Username      = String

deriving instance Read Dimension'

-- | A user of the website.
data User = User {
    _userUsername :: Username
  , _userHome     :: Maybe RegionId
  , _userPwdHash  :: PasswordHash
  } deriving (Eq, Ord, Read, Show)
makeLensesWith camelCaseFields ''User

-- | A measurable quantity like "CO2 emissions" or "Plastic tax".
data Metric = Metric {
    _metricIdent :: MetricId
  , _metricOwner :: Username
  , _metricName  :: MetricName
  , _metricDim   :: Either Model.Dimension MetricSymbol
  } deriving (Eq, Ord, Read, Show)
makeLensesWith camelCaseFields ''Metric

-- | Progress and targets for multiple metrics, under one region.
data Region = Region {
    _regionIdent    :: RegionId
  , _regionOwner    :: Username
  , _regionName     :: RegionName
  , _regionProgress :: Set ProgressId
  , _regionTargets  :: Set TargetsId
  , _regionReps     :: [RepId] -- ^ Order of importance.
  , _regionParents  :: Set RegionId
  , _regionChildren :: Set RegionId
  } deriving (Eq, Ord, Read, Show)
makeLensesWith camelCaseFields ''Region

-- | Measurements for one (metric, region).
data Progress = Progress {
    _progressIdent  :: ProgressId
  , _progressOwner  :: Username
  , _progressMetric :: MetricId
  , _progressRegion :: RegionId
  , _progressReps   :: [RepId]
  , _progressValues :: [Measurement]
  } deriving (Eq, Ord, Read, Show)
makeLensesWith camelCaseFields ''Progress

-- | A target for some metric with a description.
data Target = Target TargetValue TargetDesc String
  deriving (Eq, Ord, Read, Show)

-- |The different types of target.
data TargetValue = NumTarget MetricValue Bool | BoolTarget Bool
  deriving (Eq, Ord, Read, Show)

-- | Targets for one (metric, region).
data Targets = Targets {
    _targetsIdent  :: TargetsId
  , _targetsOwner  :: Username
  , _targetsMetric :: MetricId
  , _targetsRegion :: RegionId
  , _targetsValues :: [Target]
  } deriving (Eq, Ord, Read, Show)
makeLensesWith camelCaseFields ''Targets

-- | A contactable representative.
data Rep = Rep {
    _repIdent :: RepId
  , _repOwner :: Username
  , _repName  :: RepName
  , _repEmail :: [Email]
  , _repPhone :: [Phone]
  } deriving Show
makeLensesWith camelCaseFields ''Rep

-- | Each field is like a table in a database.
data Database = Database {
    _databaseIdent    :: ID
  , _databaseUsers    :: Map Username User
  , _databaseMetrics  :: Map MetricId Metric
  , _databaseRegions  :: Map RegionId Region
  , _databaseProgress :: Map ProgressId Progress
  , _databaseTargets  :: Map TargetsId Targets
  , _databaseReps     :: Map RepId Rep
  } deriving Show
makeLensesWith camelCaseFields ''Database
