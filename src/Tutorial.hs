module Tutorial where

-- * Example database usage (see 'example' below).

import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.State               as State
import           Data.Maybe                        (fromJust)
import qualified Database.Store.Class              as S
import           Database.Store.Store.InMemory     (InMemoryStore',
                                                    InMemoryStoreIO,
                                                    runInMemoryStore)
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI

import qualified Constructors                      as C
import           Database                          (DBOps, add, runDBOps)
import           Model
import           Pretty                            (prettyLn)

-- | Pretty print the database after running the example below.
runExample :: IO ()
runExample = do
  finale <- fromJust <$> run example2
  pure ()

-- | We are using an in-memory non-persisted data store.
type Store a = InMemoryStoreIO a

-- | The associated function to run data transactions.
run = runInMemoryStore

-- | The contents of this function show how we can modify the database.
example2 :: Store InMemoryStore'
example2 = do
  -- | Creating two users.
  gabriel <- S.set $ C.user "gabmass"        "!@£$%^&*()"
  jeremy  <- S.set $ C.user "barischrooneyj" "!@£$%^&*()"

  -- | A few regions including the root "World".
  world   <- S.set $ C.region' jeremy  "World"
  germany <- S.set $ C.region  jeremy  "Germany" world
  france  <- S.set $ C.region  gabriel "France"  world

  -- | Some metrics, not connected to regions yet.
  co2        <- S.set $ C.metric  gabriel "CO2 Emissions" (SI.liter Dim./ SI.second)
  plasticTax <- S.set $ C.metric' gabriel "Plastic Tax"   "%"

  -- | Some progress for the regions.
  S.set $ C.progress co2        germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  S.set $ C.progress co2        france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  S.set $ C.progress plasticTax france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  S.set $ C.progress plasticTax germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  (liftIO . print) =<< S.get france

  -- | CO2 targets for France.
  S.set $ C.targets co2 france [
      C.targetDecrease 35 "UN 2020"       2020 1 1
    , C.targetDecrease 30 "National Plan" 2025 5 5
    ]

  State.get
