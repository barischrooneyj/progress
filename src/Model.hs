{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}

module Model where

-- * Modeling the progress and targets of regions.
--
-- | Reading through the data types, 'Region' emerges as a central data type.
-- Everything is connected to a region, and they can be connected to each other.
-- Even though the term region invokes ideas of a physical region, the idea is
-- more broad than that. A region in this model could be quite abstract, perhaps
-- most generally considered as a set of grouped issues/targets. Regions have
-- 'Targets' and the 'Progress' they have made towards those targets.

-- ** TODO: Add HTTP API.
-- ** TODO: Suggest metrics for region via sibling regions.
-- ** TODO: Get progress of a region in terms of children.
-- ** TODO: Get progress of other regions on same metric.

import           Control.Lens
import qualified Data.DateTime             as T
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Typeable             (Typeable)
import qualified Database.Store.Class      as S
import           Numeric.Units.Dimensional (Dimension' (..))

-- | Keeping track of the imports necessary for simple-store.
import           Database.Store.Class      (Consistent, Identifiable, Storable,
                                            Update (..))

-- ** The data types.

-- | First our many type aliases!
type DateTime        = T.DateTime
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
  } deriving (Read, Show, Typeable)

makeLensesWith camelCaseFields ''User
instance Identifiable User Username where
  key u = u ^. username
instance Storable User Username
instance Consistent User Username

-- | A measurable quantity like "CO2 emissions" or "Plastic tax".
data Metric = Metric {
    _metricOwner     :: Username
  , _metricName      :: MetricName
  , _metricDimension :: MetricDimension
  } deriving (Read, Show)

makeLensesWith camelCaseFields ''Metric
instance Identifiable Metric MetricKey where
  key m = (m ^. name, m ^. dimension)
instance Storable Metric MetricKey
instance Consistent Metric MetricKey

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

makeLensesWith camelCaseFields ''Region
instance Identifiable Region RegionName where
  key r = r ^. name
instance Storable Region RegionName where
instance Consistent Region RegionName where
  -- onAdd = [updateProgress]
  --   where updateProgress = Update (a ->
  --           ())

-- | Measurements for one (metric, region).
data Progress = Progress {
    _progressIdent  :: ProgressId
  , _progressOwner  :: Username
  , _progressMetric :: MetricKey
  , _progressRegion :: RegionName
  , _progressReps   :: [RepId]
  , _progressValues :: [Measurement]
  } deriving (Read, Show)

makeLensesWith camelCaseFields ''Progress
instance Identifiable Progress ProgressKey where
  key p = (p ^. metric, p ^.region)
instance Storable Progress ProgressKey
instance Consistent Progress ProgressKey where
  onAdd = const [updateRegion]
    where updateRegion = Update (\a -> (
            -- | Add a reference to a 'Region' about this 'Progress'.
              [a ^. region],
              \(b :: Region) -> b & progress %~ Set.insert (a ^. ident))
            )

-- | A target for some metric with a description.
-- | Compared by all fields.
data Target = Target {
    _targetIncrease    :: Bool
  , _targetValue       :: MetricValue
  , _targetDescription :: TargetDesc
  , _targetDate        :: DateTime
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
instance Identifiable Targets TargetsKey where
  key t = (t ^. metric, t ^.region)
instance Storable Targets TargetsKey
instance Consistent Targets TargetsKey

-- | A contactable representative.
data Rep = Rep {
    _repIdent  :: RepId
  , _repOwner  :: Username
  , _repName   :: RepName
  , _repRegion :: RegionName
  , _repEmail  :: [Email]
  , _repPhone  :: [Phone]
  } deriving (Read, Show)

makeLensesWith camelCaseFields ''Rep
instance Identifiable Rep RepKey where
  key r = (r ^. name, r ^.region)
instance Storable Rep RepKey
