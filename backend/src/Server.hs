{-# OPTIONS_GHC -fno-warn-missing-fields #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Server for interacting with the database over a network.
module Server where

import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant                              (Application, Proxy (..), Raw,
                                                       Server, serve)
import           Servant.API                          ((:<|>) (..), (:>), Capture,
                                                       Get, JSON)
import           Servant.Server.Internal.Handler      (Handler)
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

-- | Choose between production/dev app.
app :: Config -> Application
app config = if True then corsApp config else simpleApp config

corsApp :: Config -> Application
corsApp config = logStdoutDev
    $ cors (const $ Just policy)
    $ serve (Proxy :: Proxy StaticAPI)
    $ staticFileServer config
  where
  policy = simpleCorsResourcePolicy
           { corsRequestHeaders = ["Content-Type"] }

simpleApp :: Config -> Application
simpleApp config = serve (Proxy :: Proxy StaticAPI) $ staticFileServer config

-- | Run the application on a given port and with given database.
run :: Config -> IO ()
run config = Warp.run (_configPort config) $ app config

type ProgressAPI = StoreAPI :<|> StaticAPI

progressServer :: Config -> Server ProgressAPI
progressServer config =
  (storeServer $ _configStoreConfig config) :<|> (staticFileServer config)
