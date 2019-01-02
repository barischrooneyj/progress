{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Example database usage, see the 'example' below for a block of operations.
module Tutorial where

import           Control.Monad                     (void)
import           Numeric.Units.Dimensional         as Dim
import qualified Numeric.Units.Dimensional.SIUnits as SI
import           Text.Read                         (readMaybe)

import           Telescope.Operations              (set)
import           Telescope.Monad                   (MonadScope)
import           Telescope.Source                  (SourceConfig)
import           Telescope.Source.File             (fileConfig)

import           BackendModel
import qualified Constructors                      as C
import           Model                             (Region, User)
import           Pretty                            (pretty, prettyLn)

-- * To start an interactive database session:
--
-- *     Compile the progress library with @stack ghci progress:exe:progress-exe@.
--
-- *     Bring this tutorial into scope, @:l src/Tutorial.hs@.
--
-- *     To create a database, run the 'example', and print the database:
-- >         config <- fileConfig "temp"
-- >         runScope config example
-- >         print =<< view User{} "john"
--
-- *     All the above can be accomplished in one go with:
-- >         ./interact.sh
--
-- *     Run an individual database operation like:
-- >         runScope config $ set $ C.user "geoff" "geoffspasword"

-- | Insert some example data into the data source.
example :: MonadScope m => m ()
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
