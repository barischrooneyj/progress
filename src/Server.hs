{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Server where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (ToJSON)
import           Data.Maybe                    (fromJust)
import           Database.Store.Class          (Store (..))
import           Database.Store.Store.InMemory
import           GHC.Generics                  (Generic)
import qualified Network.Wai.Handler.Warp      as Warp
import           Numeric.Units.Dimensional     (Dimension' (..))
import           Servant                       (Application, Proxy (..), Server)
import qualified Servant
import           Servant.API

import qualified Database                      as Db
import           Model

-- | Deriving necessary instances for on-the-wire serialization.
deriving instance Generic User
deriving instance ToJSON User
deriving instance Generic Metric
deriving instance ToJSON Metric
deriving instance Generic Region
deriving instance ToJSON Region
deriving instance Generic Progress
deriving instance ToJSON Progress
deriving instance ToJSON Target
deriving instance Generic Target
deriving instance Generic Targets
deriving instance ToJSON Targets
deriving instance Generic Rep
deriving instance ToJSON Rep

deriving instance ToJSON Dimension'

type API =
       "metric"   :> "all" :> Get '[JSON] [Metric]
  :<|> "user"     :> "all" :> Get '[JSON] [User]
  :<|> "region"   :> "all" :> Get '[JSON] [Region]
  :<|> "progress" :> "all" :> Get '[JSON] [Progress]
  :<|> "targets"  :> "all" :> Get '[JSON] [Targets]
  :<|> "rep"      :> "all" :> Get '[JSON] [Rep]

-- | Serve a list of all stored values of given type.
serveAll db a = fromJust <$> liftIO (Db.run db $ viewAll a)

-- | Handler that simply returns users.
server :: InMemoryStore' -> Server API
server db =
       serveAll db Metric{}
  :<|> serveAll db User{}
  :<|> serveAll db Region{}
  :<|> serveAll db Progress{}
  :<|> serveAll db Targets{}
  :<|> serveAll db Rep{}

-- | Application that serves the API.
app :: InMemoryStore' -> Application
app db = Servant.serve (Proxy :: Proxy API) $ server db

-- | Run the app on a given port.
run :: Int -> InMemoryStore' -> IO ()
run port db = Warp.run port $ app db
