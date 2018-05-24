{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Lib where

-- * Modeling the progress and targets of regions.

-- ** TODO: Suggest metrics for sibling regions.
-- ** TODO: Show progress of a region in terms of children.
-- ** TODO: Show progress of other regions on same metric.
-- ** TODO: Better date input format.
-- ** TODO: Verify Eq and Ord instances.

import           Control.Monad.State
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI
import           Text.Pretty.Simple                (pPrint)

-- ** Example usage.

runExample :: IO ()
runExample = runDBState $ do

  -- | Creating two users.
  gabriel <- addUser $ user "gabmass" "!@£$%^&*()"
  jeremy <- addUser $ user "barischrooneyj" "!@£$%^&*()"


  pure ()

-- | Creating two users.
gabriel = user "gabmass" "!@£$%^&*()"
jeremy = user "barischrooneyj" "!@£$%^&*()"

world = region jeremy "World" -- ^ I own the world.
germany = region' jeremy "Germany" world  -- ^ And Germany, child of world.
france = region' gabriel "France" world  -- ^ You own France, child of world.

-- | Two metrics.
co2 = metric gabriel "CO2 Emissions" (SI.liter Dim./ SI.second)
plasticTax = metric' gabriel "Plastic Tax" "%"

-- | Progress in those metrics for France and Germany.
co2Germany = progress co2 germany [(47, 0), (40, 10)]
co2France = progress co2 france [(45, 0), (42, 10)]
plasticTaxFrance = progress plasticTax france [(10, 0), (10, 10)]
plasticTaxGermany = progress plasticTax germany [(8, 0), (10, 10)]

-- | CO2 targets for France.
franceCo2Targets = targets co2 france [
  targetDecrease 35 "UN 2020" "1/1/2020",
  targetDecrease 30 "National Plan" "1/1/2025"]

someFunc :: IO ()
someFunc = do
  -- | Showing how we can print/parse a data type.
  let franceString = show france
      france'      = read franceString :: Region
  pPrint france'
  pPrint co2France
  pPrint franceCo2Targets

-- ** The data types.

type Dimension     = Dimension'
type DimensionName = String
type Email         = String
type Measurement   = (MetricValue, Seconds)
type MetricId      = Int
type MetricName    = String
type MetricSymbol  = String
type MetricValue   = Double
type PasswordHash  = String
type Phone         = String
type ProgressId    = Int
type RegionId      = Int
type RegionName    = String
type RepId         = Int
type RepName       = String
type Seconds       = Integer
type TargetDesc    = String
type TargetsId     = Int
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
  } deriving (Read, Show)

-- | Progress and targets for multiple metrics, under one region.
data Region = Region {
    _rgid     :: RegionId
  , _rgowner  :: Username
  , _name     :: RegionName
  , _progress :: Set ProgressId
  , _targets  :: Set TargetsId
  , _reps     :: [RepId] -- ^ Order of importance.
  , _parents  :: Set RegionId
  , _children :: Set RegionId
  } deriving (Read, Show)

-- | Measurements for one (metric, region).
data Progress = Progress {
    _pid    :: ProgressId
  , _owner  :: Username
  , _metric :: MetricId
  , _region :: RegionId
  , _reps   :: [RepId]
  , _values :: [Measurement]
  } deriving (Eq, Ord, Read, Show)

-- | Targets for one (metric, region).
data Targets = Targets {
    _tsid    :: TargetsId
  , _owner   :: Username
  , _metric  :: MetricId
  , _region  :: RegionId
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

-- ** Helper functions.

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
    _users     :: Set User
  , _metrics   :: Set Metric
  , _regions   :: Set Region
  , _progresss :: Set Progress
  , _targetss  :: Set Targets
  , _reps      :: Set Rep
  } deriving Show

-- | A database with no data.
emptyDB :: DB
emptyDB = DB Set.empty Set.empty Set.empty Set.empty Set.empty Set.empty

-- | Add a user to the database.
addUser :: User -> State DB User
addUser newUser = do
  db <- get
  put $ db { _users = Set.insert newUser (_users  db) }
  pure newUser

-- | Run the given operations and print the resulting database.
runDBState :: State DB a -> IO ()
runDBState operations = pPrint $ evalState (operations >> get) emptyDB
