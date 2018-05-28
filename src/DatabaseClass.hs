{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module DatabaseClass where

-- ** TODO: Consider reducing constraint from State to Reader.
-- ** TODO: Add monadic lens operators for Store.
-- ** TODO: Do not require user to write Entity, only Identifiable.
-- ** TODO: Factor out fullKey function.

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

-- | An Identifiable value can be uniquely determined among terms of other
-- types. The Ord constraint is required to identify within the same type, and
-- Typeable to distinguish from other types.
class (Ord k, Typeable a) => Identifiable a k | a -> k where
  key :: a -> k
  table :: a -> String
  table = const $ show $ Type.typeOf (undefined::a)

-- | To persist a term we also need to serialize and de-serialize it.
class (Identifiable a k, Read a, Show a, Show k) => Entity a k

-- | A Store can get/set/modify entities. It is like a monadic lens.
-- NOTE: This is just a simple key-value store for now.
class (Entity a k, MonadPlus m, MonadState db m) => Store db m a k where
  get  :: k             -> m a
  set  :: a             -> m a
  over :: k -> (a -> a) -> m a
  over k fa = do
    row <- get k
    set (fa row)

-- | Run the given store operations.
runStore :: Monoid m => StateT m (MaybeT IO) a -> IO (Maybe a)
runStore x = runMaybeT $ fst <$> S.runStateT x mempty

-- | A data type we want to persist.
data User = User {
    _userUsername :: String
  , _userPwdHash  :: String
  } deriving (Read, Show, Typeable)

-- | The User must be an Entity.
instance Entity User String
instance Identifiable User String where
  key = _userUsername

-- | Our in-memory database.
newtype MapDB = MapDB { _databaseMap :: Map String String }
  deriving (Show, Monoid)

-- | The in-memory database uses Data.Map and Read/Show instances.
instance (Entity a k, MonadPlus m, MonadState MapDB m) => Store MapDB m a k where
  get k = do
    db <- _databaseMap <$> S.get
    liftMaybe $ read <$> Map.lookup (table (undefined::a) ++ show k) db
  set a = do
    let fullKey = table (undefined::a) ++ show (key a)
    db <- _databaseMap <$> S.get
    if   fullKey `Map.member` db
    then mzero
    else do
      S.put $ MapDB $ Map.insert fullKey (show a) db
      pure a

-- | Choosing our database implementation.
type MyDb a = StateT MapDB (MaybeT IO) a
runMyDb :: MyDb a -> IO (Maybe a)
runMyDb = runStore

example :: MyDb User
example = do
  db <- set (User "John" "!@£$")
  liftIO $ print db
  get "John"

run :: IO (Maybe User)
run = runMyDb example
