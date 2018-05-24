{-# LANGUAGE TemplateHaskell #-}

module Constructors where

-- ** Helpful constructors for the data types.

import           Control.Lens
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import           Numeric.Units.Dimensional         as Dim

import           Model

-- | Constructor for a user.
user :: Username -> PasswordHash -> User
user u = User u Nothing

-- | Constructor for a metric.
metric :: HasDimension a => User -> MetricName -> a -> Model.Metric
metric u mn d = Model.Metric 0 (u ^. username) mn (Left $ Dim.dimension d)

-- | Constructor for a dimensionless metric.
metric' :: User -> MetricName -> MetricSymbol -> Model.Metric
metric' u mn d = Model.Metric 0 (u ^. username) mn (Right d)

-- | Constructor for an empty region.
region :: User -> RegionName -> Region
region u rn = Region 0 (u ^. username) rn Set.empty Set.empty [] Set.empty Set.empty

-- | Constructor for a region with parent.
region' :: User -> RegionName -> Region -> Region
region' u rn r =
  Region 0 (u ^. username) rn Set.empty Set.empty [] (Set.singleton $ r ^. ident) Set.empty

-- | Constructor for some progress.
progress :: Metric -> Region -> [Measurement] -> Progress
progress m r = Progress 0 (r ^. owner) (m ^. ident) (r ^. ident) []

-- | Constructor for a goal with given targets.
targets :: Metric -> Region -> [Target] -> Targets
targets m r = Targets 0 (r ^. owner) (m ^. ident) (r ^. ident)

-- | Constructors for different types of target.
targetIncrease m = Target $ NumTarget m True
targetDecrease m = Target $ NumTarget m False
targetBool     b = Target $ BoolTarget b

-- | A database with no data.
emptyDB :: DB
emptyDB = DB 0 Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

-- ** Helper functions on the data types.

-- | A readable representation of a dimension.
dimensionName :: Model.Dimension -> DimensionName
dimensionName (Dim' l m t _i _th _n _j) =
  power' "m" l ++ power' "g" m ++ power' "s" t
  where power _ 0 = ""
        power s x = show s ++ "^" ++ show x
        power' :: DimensionName -> Int -> DimensionName
        power' s x = filter (/= '\"') (power s x)

