module Config where

import           Telescope.Class      (StoreConfig, newStoreConfig)
import           Telescope.Store.File (File (File))

-- | Configuration options based on deployment.
data Config = Config {
    _configCors        :: Bool
  , _configPort        :: Int
  , _configStoreConfig :: StoreConfig File
  } deriving Show

devConfig :: IO Config
devConfig = do
  storeConfig <- newStoreConfig "dev" File
  pure $ Config True 8081 storeConfig

getConfig :: String -> IO (Maybe Config)
getConfig = \case
  "dev"     -> Just <$> devConfig
  otherwise -> pure Nothing
