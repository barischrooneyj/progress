{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Lib where

-- * Modeling region's and their issues.

import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI
import           Text.Pretty.Simple                (pPrint)

-- ** Example usage.

gabriel = user "gabmass" "!@£$%^&*()"
jeremy = user "barischrooneyj" "!@£$%^&*()"
world = region jeremy "World"
germany = region' jeremy "Germany" world
france = region' gabriel "France" world
co2 = metric gabriel "CO2 Emissions" (SI.liter Dim./ SI.second)
plasticTax = metric' gabriel "Plastic Tax" "%"
co2Germany = record co2 germany [(47, 0), (40, 10)]
co2France = record co2 france [(45, 0), (42, 10)]
plasticTaxFrance = record plasticTax france [(10, 0), (10, 10)]
plasticTaxGermany = record plasticTax germany [(8, 0), (10, 10)]
franceCo2Goal = goal co2 france [
  targetDecrease "UN 2020" "1/1/2020" 35,
  targetDecrease "National Plan" "1/1/2025" 30]

someFunc :: IO ()
someFunc = do
  let s = show france
  pPrint (read s :: Region)
  pPrint co2France
  pPrint franceCo2Goal

-- ** The data types.

type Dimension     = Dimension'
type DimensionName = String
type Email         = String
type GoalId        = Int
type Measurements  = [(MetricValue, Seconds)]
type MetricId      = Int
type MetricName    = String
type MetricSymbol  = String
type MetricValue   = Double
type PasswordHash  = String
type Phone         = String
type RecordId      = Int
type RegionId      = Int
type RegionName    = String
type RepId         = Int
type RepName       = String
type Seconds       = Integer
type TargetDesc    = String
type Username      = String

deriving instance Read Dimension'

-- | A user currently only has a username.
data User = User {
    _username :: Username
  , _home     :: Maybe RegionId
  , _pwdHash  :: PasswordHash
  } deriving (Show, Read)

-- | A measurable quantity like "CO2 emissions" or "Plastic tax".
data Metric = Metric {
    _mid   :: MetricId
  , _owner :: Username
  , _name  :: MetricName
  , _dim   :: Either Lib.Dimension MetricSymbol
  } deriving (Read, Show)

-- | Measurements and goals for multiple metrics, under one region.
data Region = Region {
    _rgid     :: RegionId
  , _rgowner  :: Username
  , _name     :: RegionName
  , _records  :: Set RecordId
  , _goals    :: Set GoalId
  , _reps     :: [RepId] -- ^ Order of importance.
  , _parent   :: Set RegionId
  , _children :: Set RegionId
  } deriving (Show, Read)

-- | Measurements for one (metric, region).
data Record = Record {
    _rcid   :: RecordId
  , _owner  :: Username
  , _metric :: MetricId
  , _region :: RegionId
  , _reps   :: [RepId]
  , _values :: Measurements
  } deriving (Eq, Ord, Read, Show)

-- | Targets for one (metric, region).
data Goal = Goal {
    _gid     :: GoalId
  , _owner   :: Username
  , _metric  :: MetricId
  , _region  :: RegionId
  , _targets :: [Target]
  } deriving (Eq, Ord, Show, Read)

-- | A target for some metric with a description.
data Target = Target TargetDesc String TargetValue
  deriving (Eq, Ord, Show, Read)

-- |The different types of target.
data TargetValue = NumTarget MetricValue Bool | BoolTarget Bool
  deriving (Eq, Ord, Show, Read)

-- | A contactable representative.
data Representative = Representative {
    _rpid  :: RepId
  , _owner :: Username
  , _name  :: RepName
  , _email :: [Email]
  , _phone :: [Phone]
  }

-- ** Helper constructors for the data types.

-- | Constructor for a user.
user :: Username -> PasswordHash -> User
user u = User u Nothing

-- | Constructor for a metric.
metric :: HasDimension a => User -> MetricName -> a -> Lib.Metric
metric u mn d = Lib.Metric 0 (_username u) mn (Left $ Dim.dimension d)

-- | Constructor for a dimensionless metric.
metric' :: User -> MetricName -> MetricSymbol -> Lib.Metric
metric' u mn d = Lib.Metric 0 (_username u) mn (Right d)

-- | Constructor for an empty region.
region :: User -> RegionName -> Region
region u rn = Region 0 (_username u) rn Set.empty Set.empty [] Set.empty Set.empty

-- | Constructor for a region with parent.
region' :: User -> RegionName -> Region -> Region
region' u rn r =
  Region 0 (_username u) rn Set.empty Set.empty [] (Set.singleton $ _rgid r) Set.empty

-- | Constructor for a record.
record :: Metric -> Region -> Measurements -> Record
record m r = Record 0 (_rgowner r) (_mid m) (_rgid r) []

-- | Constructor for a goal with given targets.
goal :: Metric -> Region -> [Target] -> Goal
goal m r = Goal 0 (_rgowner r) (_mid m) (_rgid r)

-- | Constructors for different types of target.
targetIncrease desc date m = Target desc date $ NumTarget m True
targetDecrease desc date m = Target desc date $ NumTarget m False
targetBool     desc date b = Target desc date $ BoolTarget b

-- | A readable representation of dimensions.
dimensionName :: Lib.Dimension -> DimensionName
dimensionName (Dim' l m t _i _th _n _j) =
  power' "m" l ++ power' "g" m ++ power' "s" t
  where power _ 0 = ""
        power s x = show s ++ "^" ++ show x
        power' :: DimensionName -> Int -> DimensionName
        power' s x = filter (/= '\"') (power s x)
