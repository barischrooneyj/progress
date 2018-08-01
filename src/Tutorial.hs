{-# LANGUAGE ScopedTypeVariables #-}

module Tutorial where

-- * Example database usage, see the 'example' below for a block of operations.
--
-- | To interact with the database interactively in GHCI:
--
-- * Open GHCI with @stack ghci progress:exe:progress-exe@ or other.
-- * Load this tutorial with all its useful imports, @:l src/Tutorial.hs@.
-- * Create a database, run the 'example' below, and print the database:
-- >     db <- newInMemoryStore
-- >     run example db
-- >     prettyLn db
-- * Or run an individual command:
-- >     db <- run (Db.set $ C.user "geoff" "geoffspasword") $ fromJust db

import           Control.Monad                     (void)
import qualified Database.Store.Class              as Db
import           Database.Store.Store.InMemory     (InMemoryStoreIO,
                                                    newInMemoryStore,
                                                    runInMemoryStore)
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI

import qualified Constructors                      as C
import           Pretty                            (prettyLn)

-- | A short example.
runExample :: IO ()
runExample = do
  db <- newInMemoryStore
  run example db
  prettyLn db

-- | We are using an in-memory non-persisted data store.
type Store a = InMemoryStoreIO a

-- | The associated function to run database operations.
run = runInMemoryStore

-- | The contents of this function show how we can modify the database.
example :: Store ()
example = void $ do
  -- | First create two users.
  gabriel <- Db.set $ C.user "gabmass"        "!@£$%^&*()"
  jeremy  <- Db.set $ C.user "barischrooneyj" "!@£$%^&*()"

  -- | You can pretty print in the 'Store' monad, or just normal print.
  -- liftIO $ prettyLn gabriel

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
