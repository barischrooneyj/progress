{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

module Pretty where

-- * Pretty printing class, aimed at command line usage..

import           Control.Lens
import           Data.DateTime                 as T
import           Data.Foldable                 (Foldable)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust, isJust)
import           Database.Store.Store.InMemory (InMemoryStore' (..))
import           Numeric.Units.Dimensional     (Dimension' (..))
import           Text.Read                     (readMaybe)

import           Model

-- | Our pretty printing type class.
class Pretty a where
  -- | A pretty representation of a type.
  pretty :: a -> String
  pretty = prettyN 0
  -- | Pretty with given indentation.
  prettyN :: Int -> a -> String
  prettyN n a = sn n ++ pretty a
  -- | Pretty print to standard out.
  prettyLn :: a -> IO ()
  prettyLn = putStrLn . pretty

-- | Given indentation w/wo newline.
sn n = replicate n ' '
ln n = "\n" ++ sn n

-- * Pretty instances of common types.

-- | Any 'Foldable' containing 'Pretty' things can also be 'Pretty'.
instance {-# OVERLAPPABLE #-} (Foldable f, Pretty a) => Pretty (f a) where
  prettyN n xs = concat [
      sn n ++ "["
    , concatMap (\x -> "\n" ++ prettyN (n+2) x ++ ",") xs
    , ln n ++ "]\n"
    ]

-- | We can also make 'Either' values pretty.
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyN n (Left x)  = prettyN n x
  prettyN n (Right x) = prettyN n x

-- | We can always just print a string.
instance Pretty String where
  prettyN n a = sn n ++ a

-- ** Pretty instances for our model types.

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
  prettyN n u = concat [
      sn n     ++ "User: " ++ u ^. username
    , ln (n+2) ++ "(home: ", show $ u ^. home, ")"
    ]

instance Pretty Metric where
  prettyN n u = concat [
      sn n     ++ "Metric: " ++ u ^. name
    , ln (n+2) ++ "(owner: ", u ^. owner, ")"
    , ln (n+2) ++ "(dim: ", prettyN n $ u ^. dimension, ")"
    ]

instance Pretty Region where
  prettyN n r = concat [
      sn n     ++ "Region: " ++ r ^. name
    , ln (n+2) ++ "(owner: ", r ^. owner, ")"
    , ln (n+2) ++ "(parents: ", show $ r ^. parents, ")"
    ]

instance Pretty Progress where
  prettyN n p = concat [
      sn n     ++ "Progress for: (" ++ fst (p ^. metric) ++ "," ++ p ^. region ++ ")"
    , ln (n+2) ++ "(owner: ", p ^. owner, ")"
    , ln (n+2) ++ "(values: ", prettyN (n+2) $ p ^. values, ")"
    ]

instance Pretty Target where
  prettyN n u = concat [
      sn n ++ "Target:" ++ if u ^. increase then "increase" else "decrease"
    , " to ", show $ u ^. value
    , " by ", show $ T.toGregorian' $ u ^. date
    , ln (n+2) ++ "(desc: ", show $ u ^.description, ")"
    ]

instance Pretty Targets where
  prettyN n t = concat [
      sn n     ++ "Targets for: (" ++ show (t ^. metric) ++ "," ++ show (t ^. region) ++ ")"
    , ln (n+2) ++ "owner: ", t ^. owner
    , ln (n+2) ++ "values: ", prettyN (n+4) $ t ^. values
    ]

instance Pretty Rep where
  prettyN n r = concat [
      sn n     ++ r ^. name
    , ln (n+2) ++ "(email: ", show $ r ^. email, ")"
    , ln (n+2) ++ "(phone: ", show $ r ^. phone, ")"
    ]

-- | A simple way to show the entire database without looking at types.
instance Pretty InMemoryStore' where
  prettyN n (InMemoryStore' m) =
    prettyN n $ map (prettyStoreValue n) $ Map.elems m

-- | Attempt to parse the given string as each possible type until success, then
-- return the pretty string of the parsed value.
prettyStoreValue :: Int -> String -> String
prettyStoreValue n s =
  if | isJust (readMaybe s :: Maybe Metric)
       -> prettyN n (fromJust $ readMaybe s :: Metric)
     | isJust (readMaybe s :: Maybe Region)
       -> prettyN n (fromJust $ readMaybe s :: Region)
     | isJust (readMaybe s :: Maybe Progress)
       -> prettyN n (fromJust $ readMaybe s :: Progress)
     | isJust (readMaybe s :: Maybe Targets )
       -> prettyN n (fromJust $ readMaybe s :: Targets)
     | isJust (readMaybe s :: Maybe User)
       -> prettyN n (fromJust $ readMaybe s :: User)
