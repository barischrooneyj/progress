module Database where

-- * Choosing our database backend.

import           Database.Store.Store.InMemory (InMemoryStoreIO,
                                                newInMemoryStore,
                                                runInMemoryStore)

-- | The in-memory non-persisted database..
type Store a = InMemoryStoreIO a

-- | ..the associated function to run database operations..
run = runInMemoryStore

-- | ..and the associated database constructor.
newStore = newInMemoryStore
