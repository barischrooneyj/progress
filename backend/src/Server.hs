{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Server for interacting with the database over a network.
module Server where

import qualified Control.Lens                    as L
import           Control.Monad.IO.Class          (liftIO)
import           Data.Maybe                      (fromJust)
import           Database.Store.Class
import           Database.Store.Store.InMemory   (InMemoryStore')
import qualified Network.Wai.Handler.Warp        as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant                         (Application, Proxy (..), Raw,
                                                  Server, serve)
import           Servant.API                     ((:<|>) (..), (:>), Capture,
                                                  Get, JSON)
import           Servant.Server.Internal.Handler (Handler)
import           Servant.Server.StaticFiles      (serveDirectoryWebApp)
import Network.Wai.Middleware.Cors

import           API
import           Config
import           BackendModel
import qualified Database                        as Db

-- data Env = Env { _envConfig :: Config }

-- | Relative path to the static files directory from the 'frontend' directory.
staticPath = "frontend-result/bin/frontend-exe.jsexe/"

-- | Our server consists of handlers for each API endpoint.
server :: MStore s m => s -> Server API
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
app :: MStore s m => s -> Config -> Application
app db config =
  if   _configCors config
  then corsApp db
  else serve (Proxy :: Proxy API) $ server db

corsApp :: MStore s m => s -> Application
corsApp db = logStdoutDev
    $ cors (const $ Just policy)
    $ serve (Proxy :: Proxy API) $ server db
  where
  policy = simpleCorsResourcePolicy
           { corsRequestHeaders = ["Content-Type"] }

-- | Run the application on a given port and with given database.
run :: InMemoryStore' -> Config -> IO ()
run db config = Warp.run (_configPort config) $ app db config

-- | Serve a list of all stored values of given type.
serveAll :: (MStore s m, Storable a k) =>
  s -> a -> Handler [a]
serveAll db a = fromJust <$> (liftIO $ Db.run db $ viewAll a)

-- | Serve first value of given type that satisfies the predicate.
serveFirstWhere :: (MStore s m, Storable a k) =>
  String -> s -> a -> (a -> Bool) -> Handler a
serveFirstWhere n db a f =
  (fromJust . fromJust) <$> liftIO (Db.run db $ firstWhere a f)
