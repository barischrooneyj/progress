module Config where

import           Data.Map (Map)
import qualified Data.Map as Map

-- | Runtime information which changes based on deployment.
data Config = Config {
    _configCors :: Bool
  , _configPort :: Int
  } deriving (Read, Show)

devConfig = Config True 8081

configMap :: Map String Config
configMap = Map.fromList [
  ("dev", devConfig)
  ]
