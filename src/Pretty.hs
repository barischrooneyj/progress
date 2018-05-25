{-# LANGUAGE FlexibleInstances #-}

module Pretty where

-- * Pretty printing typeclass and instances.

import           Control.Lens
import           Data.Foldable (Foldable)
import qualified Data.Map      as Map

import           Model

-- | The pretty printing type class. The aim is easily interpret-able output for
-- the command line.
class Pretty a where
  pretty :: a -> String
  prettyLn :: a -> IO ()
  prettyLn = putStrLn . pretty

-- | Any foldable of pretty things is also a foldable.
instance (Foldable f, Pretty a) => Pretty (f a) where
  pretty xs = "[\n" ++ concatMap (\x -> "  " ++ pretty x ++ ",\n") xs ++ "]"

-- ** Model instances.

instance Pretty User where
  pretty u = concat [
      u ^. username
    , " (home: ", show $ u ^. home, ")"
    ]

instance Pretty Metric where
  pretty u = concat [
      u ^. name
    , " (owner: ", u ^. owner, ")"
    ]

instance Pretty Region where
  pretty u = concat [
      u ^. name
    , " (owner: ", u ^. owner, ")"
    , " (parents: ", show $ u ^. parents, ")"
    ]

instance Pretty Progress where
  pretty u = concat [
      " (metric: ", show $ u ^. metric, ")"
    , " (region: ", show $ u ^. region, ")"
    , " (owner: ", u ^. owner, ")"
    , " ", show $ u ^. values
    ]

instance Pretty Target where
  pretty u = concat [
      if u ^. increase then "Above" else "Below", " ", show $ u ^. value
    , " by ", show $ u ^. date
    ]

instance Pretty Targets where
  pretty t = concat [
      " (metric: ", show $ t ^. metric, ")"
    , " (region: ", show $ t ^. region, ")"
    , " (owner: ", t ^. owner, ")"
    , " ", pretty $ t ^. values
    ]

instance Pretty Rep where
  pretty r = concat [
      r ^. name
    , " (email: ", show $ r ^. email, ")"
    , " (phone: ", show $ r ^. phone, ")"
    ]

instance Pretty Database where
  pretty d = concat [
      " (Users: ", pretty $ Map.elems $ d ^. users, ")"
    , " (Metrics: ", pretty $ Map.elems $ d ^. metrics, ")"
    , " (Regions: ", pretty $ Map.elems $ d ^. regions, ")"
    , " (Progress: ", pretty $ Map.elems $ d ^. progress, ")"
    , " (Targets: ", pretty $ Map.elems $ d ^. targets, ")"
    , " (Reps: ", pretty $ Map.elems $ d ^. reps, ")"
    ]
