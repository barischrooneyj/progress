{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | Server for interacting with the database over a network.
module Server where

import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.Cors          as Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant

import           Telescope.Server                     (SourceAPI, sourceServer)
import           Telescope.Source                     (Source, SourceConfig)

import           API                                  (StaticAPI)
import           Config                               (Config (..))

-- | Path to the static files directory from the 'frontend' directory.
staticPath = "frontend-result/bin/frontend-exe.jsexe/"

-- | A server for the static files built for the frontend.
staticFileServer :: Server StaticAPI
staticFileServer = serveDirectoryWebApp staticPath

-- | API combining the Telescope API and static file API.
type ProgressAPI = SourceAPI :<|> StaticAPI

-- | Server combining necessary handlers for the 'ProgressAPI'.
progressServer :: Config -> Server ProgressAPI
progressServer config =
  (sourceServer $ _configSourceConfig config) :<|> staticFileServer

-- | Application that runs the 'progressServer'.
app :: Config -> Application
app config = serve (Proxy :: Proxy ProgressAPI) $ progressServer config

-- | The application but with CORS enabled.
corsApp :: Config -> Application
corsApp config =
  logStdoutDev $ Cors.cors (const $ Just policy) $ app config
  where policy = Cors.simpleCorsResourcePolicy
          { Cors.corsRequestHeaders = ["Content-Type"] }

-- | Choose between the CORS/non-CORS application.
appMaybeCors :: Config -> Application
appMaybeCors config =
  if _configCors config then corsApp config else app config

-- | Run the application on a given port and with given database.
run :: Config -> IO ()
run config = Warp.run (_configPort config) $ appMaybeCors config
