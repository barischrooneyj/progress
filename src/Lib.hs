{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Lib where

-- * Modeling region's and their issues.

import           Data.Proxy                                     (Proxy)
import           Data.Set                                       (Set)
import qualified Data.Set                                       as Set
import           GHC.Generics
import           Numeric.Units.Dimensional                      as D
import           Numeric.Units.Dimensional.Dimensions.TermLevel as T
import           Numeric.Units.Dimensional.Dynamic
import qualified Numeric.Units.Dimensional.SIUnits              as SI

-- ** The data types.

-- | A name for a measurable quantity like "CO2 emissions" or "Plastic tax".
type MetricName = String

-- | 'Measurements' that are associated with some 'MetricName'.
-- data Record = Record { _name :: MetricName , _measurements :: Measurements }

-- | A 'Record' has a name and measurements in one dimension.
data Record = Record { _name :: MetricName , _unit :: Dimension', _measurements :: [Double] }
  deriving (Show, Read)

deriving instance Read Dimension'
deriving instance Ord Record

-- | We compare 'Record's only by 'MetricName'.
instance Eq Record where
  (Record n1 _ _) == (Record n2 _ _) = n1 == n2

-- | A collection of 'Record', each with unique name.
type Records = Set Record

-- | A simple name type for a 'Region'.
type RegionName = String

-- | A collection of 'Record', unique by name, which fall under one region.
data Region = Region { _name :: RegionName, _records :: Records, _owner :: User }
  deriving (Read, Show)

-- | We compare 'Region's only by name.
instance Eq Region where
  (Region n1 _ _) == (Region n2 _ _) = n1 == n2

-- | A collection of 'Region', each with unique name.
type Regions = Set Region

newtype User = User { _username :: String }
  deriving (Show, Read)

-- ** Helpful constructors.

-- | Constructor for a 'Record'.
-- record :: forall m d. HasDimension (Proxy d) => MetricName -> [Double] -> Unit m d Double -> Record
-- record mname measurements unit = Record mname $ Measurements (map toQuantity measurements)
--   where toQuantity x = (D.*~) x unit

-- ** Example usage.

user1 = User "somebody"
co2 = "CO2 emissions"
litresPerSecond = SI.liter D./ SI.second

-- | CO2 measurements for Ireland over time.
-- netherlandsCo2 :: Record
-- netherlandsCo2 = record co2 [109, 400, 12, 33] litresPerSecond

netherlands :: Region
-- netherlands = Region "Ireland" (Set.singleton netherlandsCo2) user1
netherlands = Region "Ireland" (Set.empty) user1

someFunc :: IO ()
someFunc = do
  -- print netherlands
  print $ dimension litresPerSecond
