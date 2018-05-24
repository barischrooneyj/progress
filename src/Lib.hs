{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Lib where

-- * Modeling region's and their issues.

import           Data.DateTime
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI
import           Text.Pretty.Simple                (pPrint)

-- ** Example usage.

-- co2 = "CO2 emissions"
-- plasticTax = "Plastic Tax"
-- co2Record = record co2 (SI.liter Dim./ SI.second)
-- plasticTaxRecord = record' plasticTax "%"
--   --   Goal co2 True [(300, "poor health"), (200, "safety limit")]
--   -- , Goal plasticTax False [(10, "somewhere"), (50, "halfway")]
-- netherlands :: Region
-- netherlands = region "Netherlands" (User "gabmass") [
--     co2Record  [350, 400, 100, 50]
--   , plasticTaxRecord  [0]
--   ] [
--   ]

gabrielId = "gabriel"
gabriel = User gabrielId Nothing "!@£$%^&*()"
co2 = Lib.Metric 0 gabrielId "CO2 emissions"
germany = Region 0 gabrielId "Germany"
co2Germany = Record 0 gabrielId 0 [] [(50, 0), (40, 10)]

someFunc :: IO ()
someFunc = do
  -- let ns = show netherlands :: String
      -- ps = read ns :: Region
  -- pPrint ps
  print "hi"

-- ** The data types.

type Dimension     = Dimension'
type DimensionName = String
type Email         = String
type GoalId        = Int
type MetricId      = Int
type MetricName    = String
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

-- | A measurable quantity like "CO2 emissions" or "Plastic tax".
data Metric = Metric {
    _id    :: MetricId
  , _owner :: Username
  , _name  :: MetricName
  , _dim   :: Either Lib.Dimension String
  }

-- | Measurements for some metric.
data Record = Record {
    _id     :: RecordId
  , _owner  :: Username
  , _metric :: MetricId
  , _reps   :: [RepId]
  , _values :: [(MetricValue, Seconds)]
  } deriving (Eq, Ord, Show, Read)

-- | Measurements and goals under one domain.
data Region = Region {
    _id      :: RegionId
  , _owner   :: Username
  , _name    :: RegionName
  , _records :: Set RecordId
  , _goals   :: Set GoalId
  , _reps    :: [RepId]
  } deriving (Show, Read)

-- | A user currently only has a username.
data User = User {
    _username :: Username
  , _home     :: Maybe RegionId
  , _pwdHash  :: PasswordHash
  } deriving (Show, Read)

-- | Targets to be reached in some metric.
data Goal = Goal {
    _id      :: GoalId
  , _owner   :: Username
  , _metric  :: MetricId
  , _targets :: [Target]
  } deriving (Eq, Ord, Show, Read)

-- | A target for some metric with a description.
data Target = Target TargetDesc TargetValue
  deriving (Eq, Ord, Show, Read)

-- |The different types of target.
data TargetValue = NumTarget (MetricValue, Bool) | BoolTarget Bool
  deriving (Eq, Ord, Show, Read)

-- | A contactable representative.
data Representative = Representative {
    _id    :: RepId
  , _owner :: Username
  , _name  :: RepName
  , _email :: [Email]
  , _phone :: [Phone]
  }

-- ** Helper functions on the data types.

-- -- | Constructor for a 'Record'.
-- record :: HasDimension a => MetricName -> a -> [MetricValue] -> Record
-- record m d a = Record m (dimension d) a Nothing

-- -- | Constructor for a dimensionless 'Record'.
-- record' :: MetricName -> String -> [MetricValue] -> Record
-- record' m s d = Record m (dimension Dim.one) d (Just s)

-- -- | Constructor for a region.
-- region :: RegionName -> User -> [Record] -> [Goal] -> Region
-- region rn owner rs gs = Region rn owner (Set.fromList rs) (Set.fromList gs)

-- | A readable representation of dimensions.
dimensionName :: Lib.Dimension -> DimensionName
dimensionName (Dim' l m t _i _th _n _j) =
  power' "m" l ++ power' "g" m ++ power' "s" t
  where power _ 0 = ""
        power s x = show s ++ "^" ++ show x
        power' :: DimensionName -> Int -> DimensionName
        power' s x = filter (/= '\"') (power s x)
