{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Server for interacting with the database over a network.
module Server where

import qualified Network.Wai.Middleware.Cors          as Cors
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Servant.Server.StaticFiles           (serveDirectoryWebApp)

import           Telescope.Class                      (StoreConfig, Store)
import           Telescope.Server
import           Telescope.Store.File                 (File)

import           API                                  (StaticAPI)
import           Config                               (Config (..))

-- | Path to the static files directory from the 'frontend' directory.
staticPath = "frontend-result/bin/frontend-exe.jsexe/"

-- | A server for static file built for the frontend.
staticFileServer :: Config -> Server StaticAPI
staticFileServer config = serveDirectoryWebApp staticPath

-- | API combining the telescope server and static file server.
type ProgressAPI = StoreAPI :<|> StaticAPI

-- | Server, combining necessary handlers for the 'ProgressAPI'
progressServer :: Config -> Server ProgressAPI
progressServer config =
  (storeServer $ _configStoreConfig config) :<|> (staticFileServer config)

-- | App application that will run the Progress server.
app :: Config -> Application
app config = serve (Proxy :: Proxy ProgressAPI) $ progressServer config

-- | The app with CORS enabled.
corsApp :: Config -> Application
corsApp config =
  logStdoutDev $ Cors.cors (const $ Just policy) $ app config
  where policy = Cors.simpleCorsResourcePolicy
          { Cors.corsRequestHeaders = ["Content-Type"] }

-- | Choose between CORS/non-CORS app.
appMaybeCors :: Config -> Application
appMaybeCors config =
  if _configCors config then corsApp config else app config

-- | Run the application on a given port and with given database.
run :: Config -> IO ()
run config = Warp.run (_configPort config) $ appMaybeCors config
