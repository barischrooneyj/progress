{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Declaration of our API.
module API where

import           Servant.API

import           Model

type StaticAPI = "static" :> Raw
