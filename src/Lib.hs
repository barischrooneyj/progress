{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Lib where

-- * Modeling the progress and targets of regions.

-- ** TODO: Suggest metrics for region via sibling regions.
-- ** TODO: Get progress of a region in terms of children.
-- ** TODO: Get progress of other regions on same metric.
-- ** TODO: Better date input format.
-- ** TODO: Correct Eq and Ord instances.

import           Control.Monad.State
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromJust)
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI
import           Text.Pretty.Simple                (pPrint)

-- ** Example usage.

runExample :: IO ()
runExample = pPrint $ runDBState example

example = do
  -- | Creating two users.
  gabriel <- add $ user "gabmass" "!@£$%^&*()"
  jeremy  <- add $ user "barischrooneyj" "!@£$%^&*()"

  -- | A few regions including the root "World".
  world   <- add $ region jeremy "World"
  germany <- add $ region' jeremy "Germany" world
  france  <- add $ region' gabriel "France" world

  -- | Some metrics, not connected to regions yet.
  co2        <- add $ metric gabriel "CO2 Emissions" (SI.liter Dim./ SI.second)
  plasticTax <- add $ metric' gabriel "Plastic Tax" "%"

  -- | Some progress for the regions.
  co2Germany        <- add $ progress co2 germany [(47, 0), (40, 10)]
  co2France         <- add $ progress co2 france [(45, 0), (42, 10)]
  plasticTaxFrance  <- add $ progress plasticTax france [(10, 0), (10, 10)]
  plasticTaxGermany <- add $ progress plasticTax germany [(8, 0), (10, 10)]

  -- | CO2 targets for France.
  franceCo2Targets <- add $ targets co2 france [
    targetDecrease 35 "UN 2020" "1/1/2020",
    targetDecrease 30 "National Plan" "1/1/2025"]

  pure ()

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
    _username :: Username
  , _home     :: Maybe RegionId
  , _pwdHash  :: PasswordHash
  } deriving (Eq, Ord, Read, Show)

-- | A measurable quantity like "CO2 emissions" or "Plastic tax".
data Metric = Metric {
    _mid   :: MetricId
  , _owner :: Username
  , _name  :: MetricName
  , _dim   :: Either Lib.Dimension MetricSymbol
  } deriving (Eq, Ord, Read, Show)

-- | Progress and targets for multiple metrics, under one region.
data Region = Region {
    _rgid      :: RegionId
  , _rgowner   :: Username
  , _name      :: RegionName
  , _progresss :: Set ProgressId
  , _targetss  :: Set TargetsId
  , _reps      :: [RepId] -- ^ Order of importance.
  , _parents   :: Set RegionId
  , _children  :: Set RegionId
  } deriving (Eq, Ord, Read, Show)

-- | Measurements for one (metric, region).
data Progress = Progress {
    _pid     :: ProgressId
  , _owner   :: Username
  , _metric  :: MetricId
  , _pregion :: RegionId
  , _reps    :: [RepId]
  , _values  :: [Measurement]
  } deriving (Eq, Ord, Read, Show)

-- | Targets for one (metric, region).
data Targets = Targets {
    _tsid    :: TargetsId
  , _owner   :: Username
  , _metric  :: MetricId
  , _tregion :: RegionId
  , _targets :: [Target]
  } deriving (Eq, Ord, Read, Show)

-- | A target for some metric with a description.
data Target = Target TargetValue TargetDesc String
  deriving (Eq, Ord, Read, Show)

-- |The different types of target.
data TargetValue = NumTarget MetricValue Bool | BoolTarget Bool
  deriving (Eq, Ord, Read, Show)

-- | A contactable representative.
data Rep = Rep {
    _rpid  :: RepId
  , _owner :: Username
  , _name  :: RepName
  , _email :: [Email]
  , _phone :: [Phone]
  } deriving Show

-- ** Constructors for the data types.

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

-- | Constructor for some progress.
progress :: Metric -> Region -> [Measurement] -> Progress
progress m r = Progress 0 (_rgowner r) (_mid m) (_rgid r) []

-- | Constructor for a goal with given targets.
targets :: Metric -> Region -> [Target] -> Targets
targets m r = Targets 0 (_rgowner r) (_mid m) (_rgid r)

-- | Constructors for different types of target.
targetIncrease m = Target $ NumTarget m True
targetDecrease m = Target $ NumTarget m False
targetBool     b = Target $ BoolTarget b

-- ** Helper functions on the data types.

-- | A readable representation of dimensions.
dimensionName :: Lib.Dimension -> DimensionName
dimensionName (Dim' l m t _i _th _n _j) =
  power' "m" l ++ power' "g" m ++ power' "s" t
  where power _ 0 = ""
        power s x = show s ++ "^" ++ show x
        power' :: DimensionName -> Int -> DimensionName
        power' s x = filter (/= '\"') (power s x)

-- ** Model of the entire program state.

-- | Currently we are using the State monad as an in-memory database.

-- | Each field is like a table in a database.
data DB = DB {
    _nextId      :: ID
  , _users       :: Set User
  , _metrics     :: Set Metric
  , _regions     :: Map RegionId Region
  , _dbProgresss :: Map ProgressId Progress
  , _dbTargetss  :: Map TargetsId Targets
  , _reps        :: Set Rep
  } deriving Show

-- | A database with no data.
emptyDB :: DB
emptyDB = DB 0 Set.empty Set.empty Map.empty Map.empty Map.empty Set.empty

-- | The next ID from the database.
nextId :: State DB ID
nextId = do
  db <- get
  put $ db { _nextId = _nextId db Prelude.+ 1 }
  pure $ _nextId db

-- | Run the given operations and print the resulting database.
runDBState :: State DB a -> DB
runDBState operations = evalState (operations >> get) emptyDB

-- ** Adding to the database.

class Add a where
  add :: a -> State DB a

-- | Add a user to the database.
instance Add User where
  add newUser = do
    db <- get
    put $ db { _users = Set.insert newUser (_users  db) }
    pure newUser

-- | Add a region to the database.
instance Add Region where
  add newRegion = do
    regionId <- nextId
    db <- get
    let newRegion' = newRegion { _rgid = regionId }
    put $ db { _regions = Map.insert regionId newRegion' (_regions  db) }
    pure newRegion'

-- | Add a metric to the database.
instance Add Metric where
  add newMetric = do
    metricId <- nextId
    db <- get
    let newMetric' = newMetric { _mid = metricId }
    put $ db { _metrics = Set.insert newMetric' (_metrics db) }
    pure newMetric'

-- | Add a progress to the database.
instance Add Progress where
  add newProgress = do
    progressId <- nextId
    db <- get
    let newProgress' = newProgress { _pid = progressId }
        region_      = fromJust $ Map.lookup (_pregion newProgress) $ _regions db
        newRegion    = region_ { _progresss = Set.insert progressId (_progresss region_) }
    put $ db {
        _dbProgresss = Map.insert progressId newProgress' (_dbProgresss db)
      , _regions   = Map.insert (_pregion newProgress) newRegion $ _regions db
      }
    pure newProgress'

-- | Add a targets to the database.
instance Add Targets where
  add newTargets = do
    targetsId <- nextId
    db <- get
    let newTargets' = newTargets { _tsid = targetsId }
        region_     = fromJust $ Map.lookup (_tregion newTargets) $ _regions db
        newRegion   = region_ { _targetss = Set.insert targetsId (_targetss region_) }
    put $ db {
        _dbTargetss = Map.insert targetsId newTargets' (_dbTargetss db)
      , _regions   = Map.insert (_tregion newTargets) newRegion $ _regions db
      }
    pure newTargets'
