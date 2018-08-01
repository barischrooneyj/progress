module Database where

-- * Our database options.

import           Database.Store.Store.InMemory (InMemoryStoreIO,
                                                runInMemoryStore)

type Store a = InMemoryStoreIO a
run = runInMemoryStore
