{-# LANGUAGE LambdaCase #-}
module Metai.Extra (debug, groupOn, minimumOn) where

import Debug.Trace (trace)
import Data.List (groupBy, minimumBy, sortOn)
import Data.Function (on)

minimumOn :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
  [] -> Nothing
  xs -> Just $ minimumBy (compare `on` f) xs

groupOn :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupOn criterion list =
  map (\group -> (fst $ head group, map snd group)) $
    groupBy ((==) `on` fst) $
      sortOn fst $
        map (\x -> (criterion x, x)) list

debug :: Show a => String -> a -> a
debug message value = trace (message ++ ": " ++ show value) value
