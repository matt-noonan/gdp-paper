{-# LANGUAGE RankNTypes #-}
module Sized (Size, the, sZipWith, sizing, align) where

newtype List n a = List a

the :: List n a -> a
the (List x) = x

sZipWith ::
  (a -> b -> c) -> List n a -> List n b -> List n c
sZipWith f xs ys = List (zipWith f (the xs) (the ys))

sizing :: [a] -> (forall n. List n a -> t) -> t
sizing xs k = k (List xs)

align :: List n a -> [b] -> Maybe (List n b)
align xs ys = if length (the xs) == length ys
              then Just (List ys)
              else Nothing
