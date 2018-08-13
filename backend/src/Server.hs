{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Server for interacting with the database over a network.
module Server where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Maybe                    (fromJust)
import           Database.Store.Class          (Store (..))
import           Database.Store.Store.InMemory (InMemoryStore')
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant                       (Application, Proxy (..), Raw,
                                                Server, serve)
import           Servant.API                   ((:<|>) (..), (:>), Get, JSON)
import           Servant.Server.StaticFiles    (serveDirectoryWebApp)

import           BackendModel
import qualified Database                      as Db


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
staticPath = "frontend-result/bin/frontend-exe.jsexe/"

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
