{-# LANGUAGE FlexibleInstances #-}

module Pretty where

-- * Pretty printing class, aimed at command line usage..

import           Control.Lens
import           Data.DateTime             as T
import           Data.Foldable             (Foldable)
import qualified Data.Map                  as Map
import           Numeric.Units.Dimensional (Dimension' (..))

import           Model

-- | Our pretty printing type class.
class Pretty a where
  pretty :: a -> String
  prettyLn :: a -> IO ()
  prettyLn = putStrLn . pretty

-- | Any foldable of pretty things is also a foldable.
instance {-# OVERLAPPABLE #-} (Foldable f, Pretty a) => Pretty (f a) where
  pretty xs = "[\n" ++ concatMap (\x -> "  " ++ pretty x ++ ",\n") xs ++ "]"

-- | We can also handle Either values.
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x)  = pretty x
  pretty (Right x) = pretty x

-- | As a fallback we can just use a string.
instance Pretty String where
  pretty = id

-- ** Model instances.

instance Pretty Measurement where
  pretty (mv, d) = show mv ++ " on " ++ show (T.toGregorian' d)

-- | A short representation like m^3 s^-1.
instance Pretty Dimension where
  pretty (Dim' l m t _i _th _n _j) = unwords [
      power' "m" l, power' "g" m, power' "s" t
    ]
    where power _ 0 = ""
          power s x = show s ++ "^" ++ show x
          power' :: DimensionName -> Int -> DimensionName
          power' s x = filter (`notElem` ['\"', ' ']) (power s x)

instance Pretty User where
  pretty u = concat [
      u ^. username
    , " (home: ", show $ u ^. home, ")"
    ]

instance Pretty Metric where
  pretty u = concat [
      u ^. name
    , " (owner: ", u ^. owner, ")"
    , " (dim: ", pretty $ u ^. dimension, ")"
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
    , " values: ", pretty $ u ^. values, ")"
    ]

instance Pretty Target where
  pretty u = concat [
      if u ^. increase then "Above" else "Below", " ", show $ u ^. value
    , " by ", show $ T.toGregorian' $ u ^. date
    , " (desc: ", show $ u ^.description, ")"
    ]

instance Pretty Targets where
  pretty t = concat [
      " (metric: ", show $ t ^. metric, ")"
    , " (region: ", show $ t ^. region, ")"
    , " (owner: ", t ^. owner, ")"
    , " (measurements: ", pretty $ t ^. values, ")"
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
