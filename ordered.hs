module Sorted (Named, SortedBy, sortBy, mergeBy) where

import The
import Named

import           Data.Coerce
import qualified Data.List       as L
import qualified Data.List.Utils as U

newtype SortedBy o a = SortedBy a
instance The (SortedBy o a) a

sortBy :: ((a -> a -> Ordering) ~~ comp)
       -> [a]
       -> SortedBy comp [a]
sortBy comp xs = coerce (L.sortBy (the comp) xs)

mergeBy :: ((a -> a -> Ordering) ~~ comp)
        -> SortedBy comp [a]
        -> SortedBy comp [a]
        -> SortedBy comp [a]
mergeBy comp xs ys =
  coerce (U.mergeBy (the comp) (the xs) (the ys))
