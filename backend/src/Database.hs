-- | Choosing our database backend.
module Database
  ( module Database.Store.Class
  , module Database
  ) where

import Database.Store.Class
import Database.Store.Store.InMemory (InMemoryStoreOps, runInMemoryStore)

-- | Store operations.
type StoreOps a = InMemoryStoreOps a

-- | The associated function to run database operations.
run = runInMemoryStore

-- | The associated database constructor.
-- newStore = newInMemoryStore
