{-# LANGUAGE ScopedTypeVariables #-}

-- | Example database usage, see the 'example' below for a block of operations.
module Tutorial where

import           Control.Monad                     (void)
import qualified Database.Store.Class              as Db
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI

import qualified Constructors                      as C
import           Database                          (Store, newStore, run)
import           Pretty                            (prettyLn)

-- * To use the database interactively in GHCI:
--
-- *     Open GHCI with @stack ghci progress:exe:progress-exe@ or other.
-- *     Load this tutorial with all its useful imports, @:l src/Tutorial.hs@.
-- *     Create a database, run the 'example' below, and print the database:
-- >         db <- newInMemoryStore
-- >         run db example
-- >         prettyLn db
-- 
-- *     For ease all the above is combined into:
-- >         ./.interact.sh
-- 
-- *     You can run an individual command like:
-- >         run db $ Db.set $ C.user "geoff" "geoffspasword"

-- | Running the example in IO.
runExample :: IO ()
runExample = do
  db <- newStore
  run db example
  prettyLn db

-- | The contents of this function show how we can modify the database.
example :: Store ()
example = void $ do
  -- First create two users.
  gabriel <- Db.set $ C.user "gabmass"        "!@£$%^&*()"
  jeremy  <- Db.set $ C.user "barischrooneyj" "!@£$%^&*()"

  -- You can pretty print in the 'Store' monad, or just normal print.
  -- liftIO $ prettyLn gabriel

  -- A few regions including the root "World".
  world   <- Db.set $ C.region' jeremy  "World"
  germany <- Db.set $ C.region  jeremy  "Germany" world
  france  <- Db.set $ C.region  gabriel "France"  world

  -- Some metrics, not connected to regions yet.
  co2        <- Db.set $ C.metric  gabriel "CO2 Emissions" (SI.liter Dim./ SI.second)
  plasticTax <- Db.set $ C.metric' gabriel "Plastic Tax"   "%"

  -- Some progress for the regions.
  Db.set $ C.progress co2        germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  Db.set $ C.progress co2        france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  Db.set $ C.progress plasticTax france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  Db.set $ C.progress plasticTax germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]

  -- CO2 targets for France.
  Db.set $ C.targets co2 france [
      C.targetDecrease 35 "UN 2020"       2020 1 1
    , C.targetDecrease 30 "National Plan" 2025 5 5
    ]
