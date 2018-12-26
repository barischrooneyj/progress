{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- | Example database usage, see the 'example' below for a block of operations.
module Tutorial where

import           Control.Monad                     (void)
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI
import           Text.Read                         (readMaybe)

import           Telescope.Class                   (MStoreA, StoreConfig, Store (..))
import           Telescope.Store.File              (File (..))

import           BackendModel
import qualified Constructors                      as C
import           Model                             (Region, User)
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
runExample :: Store s => StoreConfig s -> IO ()
runExample = const $ pure ()
  -- db <- Db.new [printYayWhenUserSet] :: IO InMemoryStore
  -- Db.run db example
  -- prettyLn db
  -- where printYayWhenUserSet s = Just $
  --         case (readMaybe s :: Maybe User) of
  --           Just user -> putStrLn $ "Yay, set " ++ pretty user
            -- Nothing   -> pure ()

-- | An example of how we can modify stored values.
example :: MStoreA ()
example = void $ do

  -- First we create two users.
  gabriel <- set $ C.user "gabmass"        "!@£$%^&*()"
  jeremy  <- set $ C.user "barischrooneyj" "!@£$%^&*()"

  -- Then create a few regions, including "Earth".
  world   <- set $ C.region' jeremy  "Earth"
  germany <- set $ C.region  jeremy  "Germany" world
  france  <- set $ C.region  gabriel "France"  world

  -- Some metrics we want to measure.
  co2        <- set $ C.metric  gabriel "CO2 Emissions" (SI.liter Dim./ SI.second)
  plasticTax <- set $ C.metric' gabriel "Plastic Tax"   "%"

  -- Record progress for the metrics for France and Germany.
  set $ C.progress co2        germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  set $ C.progress co2        france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  set $ C.progress plasticTax france  [(47, (2020, 2, 2)), (40, (2020, 4, 4))]
  set $ C.progress plasticTax germany [(47, (2020, 2, 2)), (40, (2020, 4, 4))]

  -- And set the CO2 targets for France.
  set $ C.targets co2 france [
      C.targetDecrease 35 "UN 2020"       2020 1 1
    , C.targetDecrease 30 "National Plan" 2025 5 5
    ]
