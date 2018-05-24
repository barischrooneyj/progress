module Database where

-- * Database interaction functions.

import           Control.Lens
import           Control.Monad.State (State, evalState, get, put)
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust)
import qualified Data.Set            as Set

import qualified Constructors        as C
import           Model

-- | Currently we use the State monad as an in-memory database.
type DBOps a = State DB a

-- | The next ID from the database.
nextId :: DBOps ID
nextId = do
  db <- get
  put $ db { _nextId = _nextId db Prelude.+ 1 }
  pure $ _nextId db

-- | Run the given operations and return the final database.
runDBOps :: DBOps a -> DB
runDBOps ops = evalState (ops >> get) C.emptyDB

-- | Any type which implements this typeclass can be modified in the database.
-- These functions are responsible for the integrity of the database.
class DB' a where
  add :: a -> DBOps a

-- | Add a new user to the database.
instance DB' User where
  add newUser = do
    db <- get
    put $ db { _users = Map.insert (newUser ^. username) newUser (_users  db) }
    pure newUser

-- | Add a new metric to the database.
instance DB' Metric where
  add newMetric = do
    metricId <- nextId
    db <- get
    let newMetric' = set ident metricId newMetric
    put $ db { _metrics = Map.insert metricId newMetric' (_metrics db) }
    pure newMetric'

-- | Add a new region to the database.
instance DB' Region where
  add newRegion = do
    regionId <- nextId
    db <- get
    let newRegion' = set ident regionId newRegion
    put $ db { _regions = Map.insert regionId newRegion' (_regions  db) }
    pure newRegion'

-- | Add new progress to the database.
instance DB' Progress where
  add newProgress = do
    progressId <- nextId
    db <- get
    let newProgress' = newProgress { _pid = progressId }
        region_      = fromJust $ Map.lookup (_pregion newProgress) $ _regions db
        newRegion    = over progress (Set.insert progressId) region_
    put $ db {
        _dbProgresss = Map.insert progressId newProgress' (_dbProgresss db)
      , _regions   = Map.insert (_pregion newProgress) newRegion $ _regions db
      }
    pure newProgress'

-- | Add new targets to the database.
instance DB' Targets where
  add newTargets = do
    targetsId <- nextId
    db <- get
    let newTargets' = newTargets { _tsid = targetsId }
        region_     = fromJust $ Map.lookup (_tregion newTargets) $ _regions db
        newRegion = over targetss (Set.insert targetsId) region_
    put $ db {
        _dbTargetss = Map.insert targetsId newTargets' (_dbTargetss db)
      , _regions   = Map.insert (_tregion newTargets) newRegion $ _regions db
      }
    pure newTargets'
