{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Sized
  ( Size, the, sZipWith, sizing, align
  , Zero, type (+), nil, (+++), zero_neutral, add_commutes
  , Infinite, omega, pop, infinity_absorbs
  ) where

import The
import Data.Coerce (coerce)

newtype Size n a = Size a
instance The (Size n a) a

sZipWith :: (a -> b -> c)
         -> Size n [a]
         -> Size n [b]
         -> Size n [c]
sZipWith f xs ys = Size (zipWith f (the xs) (the ys))

sizing :: [a] -> (forall n. Size n [a] -> t) -> t
sizing xs k = k (coerce xs)

align :: Size n [a] -> [b] -> Maybe (Size n [b])
align xs ys = if length (the xs) == length ys
              then Just (coerce ys)
              else Nothing

----------------------------------------------------------------

data Zero
data x + y

nil :: Size Zero [a]
nil = Size []

(+++) :: Size n [a] -> Size m [a] -> Size (n + m) [a]
xs +++ ys = coerce (the xs ++ the ys)

zero_neutral :: Size (Zero + n) a -> Size n a
zero_neutral = coerce

add_commutes :: Size (n + m) a -> Size (m + n) a
add_commutes = coerce

----------------------------------------------------------------

data Infinite

omega :: Size Infinite [Int]
omega = Size [0..]

pop :: Size Infinite [a] -> (a, Size Infinite [a])
pop (Size (x:xs)) = (x, coerce xs)

infinity_absorbs :: Size (n + Infinite) a -> Size Infinite a
infinity_absorbs = coerce
