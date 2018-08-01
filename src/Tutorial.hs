module Tutorial where

-- * Example database usage (see 'example' below).

import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.State               as State
import           Data.Maybe                        (fromJust)
import qualified Database.Store.Class              as Db
import           Database.Store.Store.InMemory     (InMemoryStore',
                                                    InMemoryStoreIO,
                                                    runInMemoryStore)
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI

import qualified Constructors                      as C
import           Data.Monoid                       (mempty)
import           Pretty                            (prettyLn)

-- | Pretty print the database after running the example below.
runExample :: IO ()
runExample = do
  let db = mempty
  -- | Run the example twice on the same database.
  db <- fromJust <$> run example db
  prettyLn db

-- | We are using an in-memory non-persisted data store.
type Store a = InMemoryStoreIO a

-- | The associated function to run data transactions.
run = runInMemoryStore

-- | The contents of this function show how we can modify the database.
example :: Store ()
example = void $ do
  -- | First create two users.
  gabriel <- Db.set $ C.user "gabmass"        "!@£$%^&*()"
  jeremy  <- Db.set $ C.user "barischrooneyj" "!@£$%^&*()"

  -- | You can print in the 'Store' monad.
  -- liftIO $ print gabriel

  -- | A few regions including the root "World".
  world   <- Db.set $ C.region' jeremy  "World"
  germany <- Db.set $ C.region  jeremy  "Germany" world
  france  <- Db.set $ C.region  gabriel "France"  world

  -- | Some metrics, not connected to regions yet.
  co2        <- Db.set $ C.metric  gabriel "CO2 Emissions" (SI.liter Dim./ SI.second)
  plasticTax <- Db.set $ C.metric' gabriel "Plastic Tax"   "%"

  -- | Some progress for the regions.
  Db.set $ C.progress co2        germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  Db.set $ C.progress co2        france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  Db.set $ C.progress plasticTax france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  Db.set $ C.progress plasticTax germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]

  -- | CO2 targets for France.
  Db.set $ C.targets co2 france [
      C.targetDecrease 35 "UN 2020"       2020 1 1
    , C.targetDecrease 30 "National Plan" 2025 5 5
    ]
