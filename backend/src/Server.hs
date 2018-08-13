{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Server for interacting with the database over a network.
module Server where

import qualified Control.Lens                    as L
import           Control.Monad.IO.Class          (liftIO)
import           Data.Maybe                      (fromJust)
import           Database.Store.Class            (Consistent, Identifiable, Store (..))
import           Database.Store.Store.InMemory   (InMemoryStore')
import qualified Network.Wai.Handler.Warp        as Warp
import           Servant                         (Application, Proxy (..), Raw,
                                                  Server, serve)
import           Servant.API                     ((:<|>) (..), (:>), Capture,
                                                  Get, JSON)
import           Servant.Server.Internal.Handler (Handler)
import           Servant.Server.StaticFiles      (serveDirectoryWebApp)

import           API
import           BackendModel
import qualified Database                        as Db

-- | Relative path to the static files directory from the 'frontend' directory.
staticPath = "frontend-result/bin/frontend-exe.jsexe/"

-- | Serve a list of all stored values of given type.
serveAll db a = fromJust <$> liftIO (Db.run db $ viewAll a)

-- | Serve first value of given type that satisfies the predicate.
serveFirstWhere :: (Consistent a k) => String ->
  InMemoryStore' -> a -> (a -> Bool) -> Handler a
serveFirstWhere n db a f =
  (fromJust . fromJust) <$> liftIO (Db.run db $ firstWhere a f)

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
  :<|> \n -> serveFirstWhere n db Region{} (\r -> r L.^. name == n)

-- | Application that combines the server and API.
app :: InMemoryStore' -> Application
app db = serve (Proxy :: Proxy API) $ server db

-- | Run the application on a given port and with given database.
run :: Int -> InMemoryStore' -> IO ()
run port db = Warp.run port $ app db
