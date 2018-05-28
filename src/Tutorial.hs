module Tutorial where

-- * Example database usage (see 'example' below).

import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI

import qualified Constructors                      as C
import           Database                          (DBOps, add, runDBOps)
import           Model
import           Pretty                            (prettyLn)

-- | Pretty print the database after running the example below.
runExample :: IO ()
runExample = do
  let finalDb  = runDBOps C.emptyDatabase example
  prettyLn finalDb

-- | The contents of this function show how we can modify the database.
example :: DBOps ()
example = do
  -- | Creating two users.
  gabriel <- add $ C.user "gabmass"        "!@£$%^&*()"
  jeremy  <- add $ C.user "barischrooneyj" "!@£$%^&*()"

  -- | A few regions including the root "World".
  world   <- add $ C.region' jeremy  "World"
  germany <- add $ C.region  jeremy  "Germany" world
  france  <- add $ C.region  gabriel "France"  world

  -- let y = key world :: RegionName
  -- | Some metrics, not connected to regions yet.
  co2        <- add $ C.metric  gabriel "CO2 Emissions" (SI.liter Dim./ SI.second)
  plasticTax <- add $ C.metric' gabriel "Plastic Tax"   "%"

  -- | Some progress for the regions.
  add $ C.progress co2        germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  add $ C.progress co2        france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  add $ C.progress plasticTax france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  add $ C.progress plasticTax germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]

  -- | CO2 targets for France.
  add $ C.targets co2 france [
      C.targetDecrease 35 "UN 2020"       2020 1 1
    , C.targetDecrease 30 "National Plan" 2025 5 5
    ]

  pure ()
