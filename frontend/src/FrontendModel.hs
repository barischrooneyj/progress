{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Instances for 'Model' data types specific to the frontend.
module FrontendModel
  ( module FrontendModel
  , module Model
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Model

deriving instance Generic User
deriving instance FromJSON User