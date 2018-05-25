module Database where

-- * Database interaction functions.

import           Control.Lens
import           Control.Monad.State (State, evalState, get, put)
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust)
import qualified Data.Set            as Set

import           Model

-- | Currently we use the State monad as an in-memory database.
type DBOps a = State Database a

-- | The next ID from the database.
nextId :: DBOps ID
nextId = do
  db <- get
  put $ db & ident +~ 1
  pure $ db ^. ident

-- | Run the given operations and return the final database.
runDBOps :: Database -> DBOps a -> Database
runDBOps database ops = evalState (ops >> get) database

-- | Any type which implements this typeclass can be modified in the database.
-- These functions are responsible for the integrity of the database.
class DB a where
  add :: a -> DBOps a

-- | Add a new user to the database.
instance DB User where
  add newUser = do
    db <- get
    put $ db & users %~ Map.insert (newUser ^. username) newUser
    pure newUser

-- | Add a new metric to the database.
instance DB Metric where
  add newMetric' = do
    metricId <- nextId
    let newMetric = newMetric' & ident .~ metricId
    db <- get
    put $ db & metrics %~ Map.insert metricId newMetric
    pure newMetric

-- | Add a new region to the database.
instance DB Region where
  add newRegion' = do
    newId <- nextId
    let newRegion = newRegion' & ident .~ newId
    db <- get
    put $ db & regions %~ Map.insert (newRegion ^. name) newRegion
    pure newRegion

-- | Add new progress to the database.
instance DB Progress where
  add newProgress' = do
    progressId <- nextId
    db <- get
    let newProgress = newProgress' & ident .~ progressId
        newRegion   = fromJust (Map.lookup (newProgress ^. region) $ db ^. regions)
                        & progress %~ Set.insert progressId
    put $ db
      & progress %~ Map.insert progressId newProgress
      & regions %~ Map.insert (newProgress ^. region) newRegion
    pure newProgress'

-- | Add new targets to the database.
instance DB Targets where
  add newTargets' = do
    targetsId <- nextId
    db <- get
    let newTargets = newTargets' & ident .~ targetsId
        newRegion  = fromJust (Map.lookup (newTargets ^. region) $ db ^. regions)
                       & targets %~ Set.insert targetsId
    put $ db
      & targets %~ Map.insert targetsId newTargets
      & regions %~ Map.insert (newTargets ^. region) newRegion
    pure newTargets'
