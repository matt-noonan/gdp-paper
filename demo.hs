{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

import Data.Coerce
import Data.Ord
import qualified Data.List as L

class The d a | d -> a where
  the :: d -> a
  default the :: Coercible d a => d -> a
  the = coerce

newtype Named name a = Named a
instance The (Named name a) a

newtype OrderedBy o a = OrderedBy a
instance The (OrderedBy o a) a

sortBy :: Named o (a -> a -> Ordering) -> [a] -> OrderedBy o [a]
sortBy c xs = coerce (L.sortBy (the c) xs)

mergeBy :: Named o (a -> a -> Ordering)
        -> OrderedBy o [a]
        -> OrderedBy o [a]
        -> OrderedBy o [a]

mergeBy c xs_ ys_ = coerce (go (the xs_) (the ys_))
  where
    go [] ys = ys
    go xs [] = xs
    go (x:xs) (y:ys) = case (the c) x y of
      GT -> y : go (x:xs) ys
      _  -> x : go xs (y:ys)

any_name :: a -> Named name a
any_name = coerce

name :: a -> (forall name. Named name a -> t) -> t
name x k = k (coerce x)

up' :: Named name (Int -> Int -> Ordering)
up' = any_name compare

down' :: Named name (Int -> Int -> Ordering)
down' = any_name (flip compare)

list1 = sortBy up'   [1..4]
list2 = sortBy down' [1..4]

merged = the (mergeBy up' list1 list2)

test =
  name compare $ \up ->
    name (flip compare) $ \down ->
      let listA = sortBy up   [1..4]
          listB = sortBy down [1..4]
      in the (mergeBy up listA listB)
