-- | Choosing our database backend.
module Database where

import           Database.Store.Store.InMemory (InMemoryStoreIO,
                                                newInMemoryStore,
                                                runInMemoryStore)

-- | The monad our operations run in.
type Store a = InMemoryStoreIO a

-- | The associated function to run database operations.
run = runInMemoryStore

-- | The associated database constructor.
newStore = newInMemoryStore
