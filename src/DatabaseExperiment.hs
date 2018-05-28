{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module DatabaseExperiment where

import           Control.Lens
import           Data.Map     (Map)
import qualified Data.Map     as Map

-- import           Model

{-
I started writing my data types and found myself writing a number of functions
corresponding to each data type, namely get and set (and derive over). So now I
have a type class for database access with functions similar to lenses.

data Foo ...

-- | NOTE: Simplified.
class Database db m a k where
  set  :: a ->             db -> m db
  get  :: a ->             db -> m a
  over :: k -> (a -> a) -> db -> m db

instance Database Foo where ...

One problem when writing out my data types was that I was including database
information in the data types. I was specifying an identifier field, and also
had to use reference keys as opposed to the original data type.

-- | Each foo
data Foo = Foo {
    _fooKey    :: Key
  , _fooValueA :: Int
  , _fooValueB :: Int
  , _fooBars   :: Keys Bar
  }

For each data type I also had to consider equality, I found myself writing the
same repetitive code. For each data type something like the below, and should I
write the same equality logic in the 'Database' code?

instance Eq Foo where
  a == b = a <= b && a >= b
instance Ord Foo where
  compare = comparing (\x -> (x ^. valueA, x ^. valueB))

Some of this pain can be taken away by existing ORM strategies like persistent,
groundhog or opaleye. Yet I wanted to explore how far we can go without Template
Haskell features, though I think it could finish this idea off nicely.

The Eq and Ord instances can be theoretically derived from knowledge of which
fields, K, make it unique, valueA and valueB above. So I shouldn't have to write
them. Similarly, knowledge of what K is, is enough to store and retrieve values.

For each data type we shouldn't need to do more than implement a HasKey type
class (below or similar) for each data type we want to store in a database, and
a Template Haskell step to write additional code. Here we attempt to distill the
difference.

instance HasKey Foo (Int, Int) where
  key foo = (foo ^. valueA, foo ^. valueB)

My plan is to write the instances for two types of database (in-memory and SQL)
and see how the type classes hold up. First the database would only be a limited
key-value store, but later the Database typeclass could be extended to be more
like groundhog's PersistEntity. I would like to see a monadic-lens provided to
the user to modify a datatype, but the true modifications are happening in the
database monad and are composable as such.

Plan:
- Working in-memory database
- Working SQL database
- Derive Eq A and Ord A instances from a key (database independent)
- Derive Database D A instances for some D (simple key-value store)
- Provide a monadic-lens API for Database operations
- Encode additional constraints (relationships) in Entity

-}

-- type Storable a = (Read a, Show a) => a

-- | Any read/show-able 'a' with a key can be stored in a table.
-- These are the requirements for being in the database.
class (Eq k, Ord k) => HasKey a k | a -> k where
  key :: a -> k

-- | A database 't' is a collection of values 'v' with keys 'k'.
-- | The database, independent of any entity, only concerned with type classes.
class Database t a k | t -> a, t -> k where
  setTRow :: a -> t -> Maybe t
  getTRow :: k -> t -> Maybe a
  overTRow :: k -> (a -> a) -> t -> Maybe t
  overTRow k fa t = do
    row <- getTRow k t
    setTRow (fa row) t

-- | An entity 'a' must have a table (be accessible) in a database.
-- | How we tell a database about an entity, connecting the two.
class (Database t a k, HasKey a k) => Entity db t a k | a -> k, t -> a, t -> k, a db -> t where
  getTable :: db -> Maybe t
  setTable :: db -> t -> Maybe db
  -- setDbRow :: a -> db -> Maybe db
  -- setDbRow a db = fmap (setTable db) (setTRow a $ getTable db)
  -- getDbRow :: k -> db -> Maybe a
  -- getDbRow k db = getTRow k =<< getTable db
  -- overDbRow :: k -> (a -> a) -> db -> Maybe db
  -- -- overDbRow k fa db = fmap (setTable db) (overTRow k fa (getTable db))

-- ** We define an in-memory database.

type Username = String
data User = User { _userUsername :: Username } deriving (Read, Show)
makeLensesWith camelCaseFields ''User

instance HasKey User Username where
  key r = r ^. username

-- | Each field is like a table in a database.
newtype MemStore = MemStore {
    _memStore :: Map String String
  } deriving Show
makeLensesWith camelCaseFields ''MemStore

-- -- | A in-memory database can use maps as tables.
-- instance (HasKey a k) => Database (Map k a) a k where
--   setTRow a m =
--     if   (show $ key a) `Map.member` m
--     then Nothing -- ^ Already in the table.
--     else Just $ Map.insert (show $ key a) a m
--   getTRow = Map.lookup

-- instance Entity MemStore (Map Username User) User Username where
--   -- getTable :: MemStore -> Map Username User
--   getTable db = Just $ db ^. users
--   setTable db table = Just $ set users table db
