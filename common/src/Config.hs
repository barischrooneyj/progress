module Config where

import           Telescope.Source      (SourceConfig)
import           Telescope.Source.File (fileConfig)

-- | Configuration options based on deployment.
data Config = Config {
    _configCors         :: Bool
  , _configPort         :: Int
  , _configSourceConfig :: SourceConfig
  } deriving Show

devConfig :: IO Config
devConfig = do
  sourceConfig <- fileConfig "dev"
  pure $ Config True 8081 sourceConfig

getConfig :: String -> IO (Maybe Config)
getConfig = \case
  "dev"     -> Just <$> devConfig
  otherwise -> pure Nothing
