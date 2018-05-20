module Lists (sortBy, mergeBy) where

import qualified Data.List as L

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = L.sortBy

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy comp = go
  where
    go [] ys = ys
    go xs [] = xs
    go xs@(x:xs') ys@(y:ys') = case comp x y of
      GT -> y : go xs  ys'
      _  -> x : go xs' ys
