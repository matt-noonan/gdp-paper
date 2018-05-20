module Examples.Sorting where

import Sorted
import The
import Named

import Data.Ord

minimum_O1 :: SortedBy comp [a] -> Maybe a
minimum_O1 xs = case the xs of
  []    -> Nothing
  (x:_) -> Just x

sort_demo :: IO ()
sort_demo = do
  xs <- readLn :: IO [Int]
  ys <- readLn
  name (comparing Down) $ \gt -> do
    let xs' = sortBy gt xs
        ys' = sortBy gt ys
    print (the xs')
    print (the ys')
    print (the $ mergeBy gt xs' ys')
