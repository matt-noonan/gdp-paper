{-# LANGUAGE RankNTypes #-}
module Sized
  (Size, the, sZipWith, sizing, align) where

newtype Size n a = Size a

the :: Size n a -> a
the (Size x) = x

sZipWith :: (a -> b -> c)
         -> Size n [a]
         -> Size n [b]
         -> Size n [c]
sZipWith f xs ys =
  Size (zipWith f (the xs) (the ys))

sizing :: [a] -> (forall n. Size n [a] -> t) -> t
sizing xs k = k (Size xs)

align :: Size n [a] -> [b] -> Maybe (Size n [b])
align xs ys = if length (the xs) == length ys
              then Just (Size ys)
              else Nothing

