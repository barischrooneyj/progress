{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

-- | Pretty printing typeclass and model instances, aimed at command line usage.
module Pretty where

import           Control.Concurrent.MVar       (readMVar)
import           Control.Lens
import           Data.DateTime                 as T
import           Data.Foldable                 (Foldable)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust, isJust)
import           Database.Store.Store.InMemory (InMemoryStore' (..))
import           Numeric.Units.Dimensional     (Dimension' (..))
import           Text.Read                     (readMaybe)

import           Model

-- | Our pretty printing typeclass.
class Pretty a where
  -- | A pretty representation of a type.
  pretty :: a -> String
  pretty = prettyN 0
  -- | Pretty with given spacesation.
  prettyN :: Int -> a -> String
  prettyN n a = sn n ++ pretty a
  -- | Pretty print to standard out.
  prettyLn :: a -> IO ()
  prettyLn = putStrLn . pretty

-- | Given amount of indent WITHOUT a newline.
sn n = replicate n ' '

-- | Given amount of indent WITH a newline.
ln n = "\n" ++ sn n

-- | The amount of spaces.
spaces = 4

-- * Pretty instances for common types/typeclasses.

-- | Any 'Foldable' containing 'Pretty' things is also 'Pretty'.
instance {-# OVERLAPPABLE #-} (Foldable f, Pretty a) => Pretty (f a) where
  prettyN n xs = concat [
      sn n ++ "["
    , concatMap (\x -> "\n" ++ prettyN (n+spaces) x ++ ",") xs
    , ln n ++ "]\n"
    ]

-- | We can also make 'Either' values pretty.
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyN n (Left x)  = prettyN n x
  prettyN n (Right x) = prettyN n x

-- | Of course we can print a string.
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
    , ln (n+spaces) ++ "(home: ", show $ u ^. home, ")"
    ]

instance Pretty Metric where
  prettyN n u = concat [
      sn n     ++ "Metric: " ++ u ^. name
    , ln (n+spaces) ++ "(owner: ", u ^. owner, ")"
    , ln (n+spaces) ++ "(dim: ", prettyN n $ u ^. dimension, ")"
    ]

instance Pretty Region where
  prettyN n r = concat [
      sn n     ++ "Region: " ++ r ^. name
    , ln (n+spaces) ++ "(owner: ", r ^. owner, ")"
    , ln (n+spaces) ++ "(parents: ", show $ r ^. parents, ")"
    ]

instance Pretty Progress where
  prettyN n p = concat [
      sn n     ++ "Progress for: (" ++ fst (p ^. metric) ++ "," ++ p ^. region ++ ")"
    , ln (n+spaces) ++ "(owner: ", p ^. owner, ")"
    , ln (n+spaces) ++ "(values: ", prettyN (n+spaces) $ p ^. values, ")"
    ]

instance Pretty Target where
  prettyN n u = concat [
      sn n ++ "Target:" ++ if u ^. increase then "increase" else "decrease"
    , " to ", show $ u ^. value
    , " by ", show $ T.toGregorian' $ u ^. date
    , ln (n+spaces) ++ "(desc: ", show $ u ^.description, ")"
    ]

instance Pretty Targets where
  prettyN n t = concat [
      sn n     ++ "Targets for: (" ++ show (t ^. metric) ++ "," ++ show (t ^. region) ++ ")"
    , ln (n+spaces) ++ "owner: ", t ^. owner
    , ln (n+spaces) ++ "values: ", prettyN (n+4) $ t ^. values
    ]

instance Pretty Rep where
  prettyN n r = concat [
      sn n     ++ r ^. name
    , ln (n+spaces) ++ "(email: ", show $ r ^. email, ")"
    , ln (n+spaces) ++ "(phone: ", show $ r ^. phone, ")"
    ]

-- | A simple way to show the entire database without looking at types.
instance Pretty InMemoryStore' where
  prettyLn (InMemoryStore' mapMVar) = do
    db <- readMVar mapMVar
    -- We subtract 'spaces' amount of spaces here to have each element in
    -- line with the opening list. This means all elements are aligned to the
    -- left margin when printing an entire database.
    putStrLn $ prettyN (-spaces) $ map (prettyStoreValue 0) $ Map.elems db

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
