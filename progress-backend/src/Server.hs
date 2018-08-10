{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Server for interacting with the database over a network.
module Server where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (ToJSON)
import           Data.Maybe                    (fromJust)
import           Database.Store.Class          (Store (..))
import           Database.Store.Store.InMemory (InMemoryStore')
import           GHC.Generics                  (Generic)
import qualified Network.Wai.Handler.Warp      as Warp
import           Numeric.Units.Dimensional     (Dimension' (..))
import           Servant                       (Application, Proxy (..), Raw,
                                                Server, serve)
import           Servant.API                   ((:<|>) (..), (:>), Get, JSON)
import           Servant.Utils.StaticFiles     (serveDirectoryWebApp)

import qualified Database                      as Db
import           Model

-- | Deriving necessary instances for serialization.
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

-- | The entire API.
type API =
       "metric"   :> "all" :> Get '[JSON] [Metric]
  :<|> "user"     :> "all" :> Get '[JSON] [User]
  :<|> "region"   :> "all" :> Get '[JSON] [Region]
  :<|> "progress" :> "all" :> Get '[JSON] [Progress]
  :<|> "targets"  :> "all" :> Get '[JSON] [Targets]
  :<|> "rep"      :> "all" :> Get '[JSON] [Rep]
  :<|> "static"   :> Raw

-- | Serve a list of all stored values of given type.
serveAll db a = fromJust <$> liftIO (Db.run db $ viewAll a)

-- | Relative path to the static files directory, from the 'progress-frontend'
-- directory, where the executable should be run.
staticPath = "../frontend-result/bin/progress-frontend-exe.jsexe/"

-- | Our server consists of handlers for each API endpoint.
server :: InMemoryStore' -> Server API
server db =
       serveAll db Metric{}
  :<|> serveAll db User{}
  :<|> serveAll db Region{}
  :<|> serveAll db Progress{}
  :<|> serveAll db Targets{}
  :<|> serveAll db Rep{}
  :<|> serveDirectoryWebApp staticPath

-- | Application that combines the server and API.
app :: InMemoryStore' -> Application
app db = serve (Proxy :: Proxy API) $ server db

-- | Run the application on a given port and with given database.
run :: Int -> InMemoryStore' -> IO ()
run port db = Warp.run port $ app db
