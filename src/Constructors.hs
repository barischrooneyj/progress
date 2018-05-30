module Constructors where

-- * Helpful constructors for the data types in Model.hs.

import           Control.Lens
import           Data.DateTime             as T
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import           Numeric.Units.Dimensional (HasDimension)
import qualified Numeric.Units.Dimensional as Dim

import           Model

-- | Constructor for a user.
user :: Username -> PasswordHash -> User
user u = User u Nothing

-- | Constructor for a metric with dimension (like m/s).
metric :: Dim.HasDimension a => User -> MetricName -> a -> Metric
metric u mn d = Metric 0 (u ^. username) mn (Left $ Dim.dimension d)

-- | Constructor for a dimensionless metric (like percent).
metric' :: User -> MetricName -> MetricSymbol -> Metric
metric' u mn d = Metric 0 (u ^. username) mn (Right d)

-- | Constructor for an empty region.
region' :: User -> RegionName -> Region
region' u rn = Region 0 (u ^. username) rn Set.empty Set.empty [] Set.empty Set.empty

-- | Constructor for a region with a parent.
region :: User -> RegionName -> Region -> Region
region u rn r = region' u rn & parents .~ Set.singleton (r ^. name)

-- | Constructor for a region's progress in some metric.
progress :: Metric -> Region -> [(MetricValue, (Integer, Int, Int))] -> Progress
progress met rg ms = Progress 0 (rg ^. owner) (met ^. ident) (rg ^. name) [] ms'
  where ms' = map (\(mv, (y, m, d)) -> (mv, T.fromGregorian' y m d)) ms

-- | Constructor for a region's targets in some metric.
targets :: Metric -> Region -> [Target] -> Targets
targets m r = Targets 0 (r ^. owner) (m ^. ident) (r ^. name)

-- | Constructors for a target in Gregorian (y m d) time.
target :: Bool -> MetricValue -> TargetDesc -> Integer -> Int -> Int -> Target
target increase value description y m d = Target {
    _targetIncrease = increase
  , _targetValue = value
  , _targetDescription = description
  , _targetDate = T.fromGregorian' y m d
  }
targetDecrease = target False
targetIncrease = target True

-- | A database with no data.
emptyDatabase :: Database
emptyDatabase = Database 0 Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

