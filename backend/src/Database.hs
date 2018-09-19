-- | Choosing our database backend.
module Database
  ( module Database.Store.Class
  , module Database
  ) where

import Database.Store.Class
import Database.Store.Store.InMemory (InMemoryStoreOps)

-- | Store operations.
type StoreOps a = InMemoryStoreOps a
