-- | Helpful constructors for the data types in Model.hs.
module Constructors where

import           Control.Lens
import qualified Data.Set                  as Set
import           Data.Time.Calendar        (fromGregorian)
import           Database.Store.Class      (Identifiable (key))
import qualified Numeric.Units.Dimensional as Dim

import           BackendModel

-- | Constructor for a user.
user :: Username -> PasswordHash -> User
user u = User u Nothing

-- | Constructor for a metric with dimension (like m/s).
metric :: Dim.HasDimension a => User -> MetricName -> a -> Metric
metric u mn d = Metric (u ^. username) mn (Left $ Dim.dimension d)

-- | Constructor for a dimensionless metric (like percent).
metric' :: User -> MetricName -> MetricSymbol -> Metric
metric' u mn d = Metric (u ^. username) mn (Right d)

-- | Constructor for an empty region.
region' :: User -> RegionName -> Region
region' u rn = Region 0 (u ^. username) rn Set.empty Set.empty [] Set.empty Set.empty

-- | Constructor for a region with a parent.
region :: User -> RegionName -> Region -> Region
region u rn r = region' u rn & parents .~ Set.singleton (r ^. name)

-- | Constructor for a region's progress in some metric.
progress :: Metric -> Region -> [(MetricValue, (Integer, Int, Int))] -> Progress
progress met rg ms = Progress 0 (rg ^. owner) (key met) (rg ^. name) [] ms'
  where ms' = map (\(mv, (y, m, d)) -> (mv, fromGregorian y m d)) ms

-- | Constructor for a region's targets in some metric.
targets :: Metric -> Region -> [Target] -> Targets
targets m r = Targets 0 (r ^. owner) 0 (r ^. name)

-- | Constructors for a target in Gregorian (y m d) time.
target :: Bool -> MetricValue -> TargetDesc -> Integer -> Int -> Int -> Target
target increase value description y m d = Target {
    _targetIncrease = increase
  , _targetValue = value
  , _targetDescription = description
  , _targetDate = fromGregorian y m d
  }
targetDecrease = target False
targetIncrease = target True
