{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module DatabaseClass where

import           Control.Monad       (MonadPlus, mzero)
import           Control.Monad.Extra (liftMaybe)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Typeable       (Typeable)
import qualified Data.Typeable       as Type

-- ** What the entity must implement.

-- | A data type we want to persist.
data User = User {
    _userUsername :: String
  , _userPwdHash  :: String
  } deriving (Read, Show, Typeable)

-- | A data type needs a key for the database.
class (Eq k, Ord k, Read a, Show a, Show k, Typeable a) => Entity a k | a -> k where
  key :: a -> k
  table :: a -> String
  table = const $ show $ Type.typeOf (undefined::a)

-- | We need to point out the key of 'User'.
instance Entity User String where
  key = _userUsername

-- ** What the database must implement.

-- | A database can get/set/modify entities.
-- Noting this is just a simple key-value store for now.
class (MonadPlus m, Entity a k) => IsDatabase db m a k where
  -- | I'd like this first get method..
  get  :: k             -> db -> m a
  set  :: a             -> db -> m db
  over :: k -> (a -> a) -> db -> m db
  over k fa t = do
    row <- get k t
    set (fa row) t

-- | An in-memory database.
newtype Database = Database { _databaseMap :: Map String String }

-- | This in-memory database uses read/show instances.
instance (MonadPlus m, Entity a k) => IsDatabase Database m a k where
  get k db = liftMaybe $ read <$> Map.lookup (table (undefined::a) ++ show k) (_databaseMap db)
  set a db =
    if   show (key a) `Map.member` _databaseMap db
    then pure $ Database $ Map.insert (show $ key a) (show a) (_databaseMap db)
    else mzero

x :: Maybe User
x = do
  db <- set (User "John" "!@£$") (Database Map.empty)
  get "John" db
