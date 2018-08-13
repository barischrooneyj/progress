{-# LANGUAGE StandaloneDeriving     #-}

-- | Modeling the progress and targets of regions.
module Model where

import           Data.Set                  (Set)
import           Data.Time.Calendar        (Day)
import           Numeric.Units.Dimensional (Dimension' (..))

-- | First our many type aliases!
type DateTime        = Day
type Dimension       = Dimension'
type DimensionName   = String
type Email           = String
type ID              = Integer
type Measurement     = (MetricValue, DateTime)
type MetricDimension = Either Dimension MetricSymbol
type MetricKey       = (MetricName, MetricDimension)
type MetricId        = ID
type MetricName      = String
type MetricSymbol    = String
type MetricValue     = Double
type PasswordHash    = String
type Phone           = String
type ProgressKey     = (MetricKey, RegionName)
type ProgressId      = ID
type RegionId        = ID
type RegionName      = String
type RepKey          = (RepName, RegionName)
type RepId           = ID
type RepName         = String
type TargetDesc      = String
type TargetsKey      = (MetricId, RegionName)
type TargetsId       = ID
type Username        = String

deriving instance Read Dimension'

-- | A user of the website.
data User = User {
    _userUsername :: Username
  , _userHome     :: Maybe RegionName
  , _userPwdHash  :: PasswordHash
  } deriving (Read, Show)

-- | A measurable quantity like "CO2 emissions" or "Plastic tax".
data Metric = Metric {
    _metricOwner     :: Username
  , _metricName      :: MetricName
  , _metricDimension :: MetricDimension
  } deriving (Read, Show)

-- | Progress and targets for multiple metrics, under one region. Regions are
-- unique by name, so there can only be one "France" or "EU".
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

-- | Measurements for one (metric, region).
data Progress = Progress {
    _progressIdent  :: ProgressId
  , _progressOwner  :: Username
  , _progressMetric :: MetricKey
  , _progressRegion :: RegionName
  , _progressReps   :: [RepId]
  , _progressValues :: [Measurement]
  } deriving (Read, Show)

-- | A target for some metric with a description.
-- Compared by all fields.
data Target = Target {
    _targetIncrease    :: Bool
  , _targetValue       :: MetricValue
  , _targetDescription :: TargetDesc
  , _targetDate        :: DateTime
  } deriving (Eq, Ord, Read, Show)

-- | Targets for one (metric, region).
data Targets = Targets {
    _targetsIdent  :: TargetsId
  , _targetsOwner  :: Username
  , _targetsMetric :: MetricId
  , _targetsRegion :: RegionName
  , _targetsValues :: [Target]
  } deriving (Read, Show)

-- | A contactable representative.
data Rep = Rep {
    _repIdent  :: RepId
  , _repOwner  :: Username
  , _repName   :: RepName
  , _repRegion :: RegionName
  , _repEmail  :: [Email]
  , _repPhone  :: [Phone]
  } deriving (Read, Show)
