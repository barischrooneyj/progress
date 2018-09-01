{-# LANGUAGE ScopedTypeVariables #-}

-- | Example database usage, see the 'example' below for a block of operations.
module Tutorial where

import           Control.Monad                     (void)
import qualified Database.Store.Class              as Db
import           Database.Store.Store.InMemory     (InMemoryStore' (..))
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI
import           Text.Read                         (readMaybe)

import           BackendModel
import qualified Constructors                      as C
import           Database                          (StoreOps (..), run)
import           Model                             (User)
import           Pretty                            (pretty, prettyLn)

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
-- >         ./interact.sh
--
-- *     You can run an individual command like:
-- >         run db $ Db.set $ C.user "geoff" "geoffspasword"

-- | A short example, similar to above. We create a database, but with an event
-- handler that prints a message whenever a 'User' is set in the database. Then
-- we run a few operation, namely the 'example' below and also print the entire
-- database contents.
runExample :: IO ()
runExample = do
  db <- Db.new [printYayWhenUserSet]
  run db example
  prettyLn db
  where printYayWhenUserSet s = Just $
          case (readMaybe s :: Maybe User) of
            Just user -> putStrLn $ "Yay, set " ++ pretty user
            Nothing   -> putStrLn "Not User"

-- | The contents of this function show how we can modify the database.
example :: StoreOps ()
example = void $ do
  -- First create two users.
  gabriel <- Db.set $ C.user "gabmass"        "!@£$%^&*()"
  jeremy  <- Db.set $ C.user "barischrooneyj" "!@£$%^&*()"

  -- You can also perform IO in the 'Store' monad, such as pretty printing a
  -- 'User'.
  -- liftIO $ prettyLn gabriel

  -- A few regions including the root "World".
  world   <- Db.set $ C.region' jeremy  "World"
  germany <- Db.set $ C.region  jeremy  "Germany" world
  Db.set $ C.region  gabriel "France"  world

  -- Some metrics, not connected to regions yet.
  -- co2        <- Db.set $ C.metric  gabriel "CO2 Emissions" (SI.liter Dim./ SI.second)
  -- plasticTax <- Db.set $ C.metric' gabriel "Plastic Tax"   "%"

  -- Some progress for the regions.
  -- Db.set $ C.progress co2        germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  -- Db.set $ C.progress co2        france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  -- Db.set $ C.progress plasticTax france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  -- Db.set $ C.progress plasticTax germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]

  -- CO2 targets for France.
  -- Db.set $ C.targets co2 france [
  --     C.targetDecrease 35 "UN 2020"       2020 1 1
  --   , C.targetDecrease 30 "National Plan" 2025 5 5
  --   ]
