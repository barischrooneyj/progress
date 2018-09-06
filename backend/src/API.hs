{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Declaration of our API.
module API where

import           Servant.API

import           Model

type API =
       "metric"   :> "all" :> Get '[JSON] [Metric]
  :<|> "user"     :> "all" :> Get '[JSON] [User]
  :<|> "region"   :> "all" :> Get '[JSON] [Region]
  :<|> "progress" :> "all" :> Get '[JSON] [Progress]
  :<|> "targets"  :> "all" :> Get '[JSON] [Targets]
  :<|> "rep"      :> "all" :> Get '[JSON] [Rep]
  :<|> "static"   :> Raw
  :<|> "region"   :> "name" :> ReqBody '[JSON] String
                  :> Post '[JSON] Region

