{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module DatabaseClass where

-- ** Experimental code for helping write database instances.

-- ** TODO: Consider reducing constraint from State to Reader.
-- ** TODO: Add monadic lens operators to Store class.
-- ** TODO: Only require user to write one instance per data type.
-- ** TODO: Factor out fullKey in Store instance.
-- ** TODO: Can we remove monoid constraint? We only want mempty.
-- ** TODO: Factor out the monad constraints in Store.

import qualified Control.Lens              as L
import           Control.Lens              (makeLensesWith, camelCaseFields)
import           Control.Monad             (MonadPlus, mzero)
import qualified Control.Monad.State       as S
import           Control.Monad.State       (StateT, MonadState)
import           Control.Monad.Extra       (liftMaybe)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.Map                  as Map
import           Data.Map                  (Map)
import           Data.Monoid               (mempty)
import           Data.Typeable             (Typeable)
import qualified Data.Typeable             as Type

-- | An 'Identifiable' is uniquely determined among terms of arbitrary type.
--
-- The 'Ord' constraint is required to identify within the same type, and
-- 'Typeable' is to distinguish from other types.
class (Ord k, Typeable a) => Identifiable a k | a -> k where
  key :: a -> k
  table :: a -> String
  table = const $ show $ Type.typeOf (undefined::a)

-- | To store a term we also need to serialize and de-serialize it.
class (Identifiable a k, Read a, Show a, Show k) => Storable a k

-- | A 'Store' can get/set/over entities. Think of it as a monadic lens.
--
-- Currently a 'Store' is just a simple key-value store, operating at the
-- granularity of single records of type 'a'. I would like to generate similar
-- lens functionality at the granularity of each field of 'a', similar to
-- 'L.makeLenses'. Currently the function 'over' can be used to perform a
-- monadic get, apply a modification, and then monadic set the result. I hope
-- that eventually our monadic lenses and standard lenses have the same API.
class (Storable a k, MonadPlus m, MonadState db m) => IsStore db m a k where
  get  :: k             -> m a
  set  :: a             -> m a
  over :: k -> (a -> a) -> m a
  over k fa = do
    row <- get k
    set (fa row)

-- | We provide a monad to run 'Store' operations.
-- Other monads can be used if they satisfy certain constraints.
type Store m a = StateT m (MaybeT IO) a

-- | ..and a method of running the monad.
runStore :: Monoid m => StateT m (MaybeT IO) a -> IO (Maybe a)
runStore x = runMaybeT $ fst <$> S.runStateT x mempty

-- ** A 'Store' implementation based in-memory.

-- | An in-memory store based on Data.Map.
newtype InMemStore = InMemStore { _databaseMap :: Map String String }
  deriving (Show, Monoid)

-- | The 'Store' instance for the in-memory store 'MemStore''.
instance (Storable a k, MonadPlus m, MonadState InMemStore m) => IsStore InMemStore m a k where
  get k = do
    db <- _databaseMap <$> S.get
    liftMaybe $ read <$> Map.lookup (table (undefined::a) ++ show k) db
  set a = do
    let fullKey = table (undefined::a) ++ show (key a)
    db <- _databaseMap <$> S.get
    if   fullKey `Map.member` db
    then mzero
    else do
      S.put $ InMemStore $ Map.insert fullKey (show a) db
      pure a

-- | Useful type aliases for the in-memory store 'MemStore'.
type InMemStore' a = Store InMemStore a
runInMemStore :: InMemStore' a -> IO (Maybe a)
runInMemStore = runStore

-- ** Library user code.

-- | A data type we want to store.
data User = User {
    _userUsername :: String
  , _userPwdHash  :: String
  } deriving (Read, Show, Typeable)
makeLensesWith camelCaseFields ''User
instance Storable User String
instance Identifiable User String where
  key = _userUsername

-- | Decide on our 'Store' implementation.
type OurStore a = InMemStore' a
run = runInMemStore

example :: OurStore User
example = do
  db <- set $ User "John" "1234"
  liftIO $ print db
  get "John"
