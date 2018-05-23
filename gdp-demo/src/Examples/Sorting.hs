{-# OPTIONS_GHC -fplugin=Tableaux #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Examples.Sorting where

import Sorted
import The
import Named
import Tableaux
import Propositional

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


test :: And p q -> Or p q
test = mp tableaux

test3 :: ( ((p `Or` q) `And` (p `Or` r)) `Impl` (p `Or` (q `And` r))) `And` ((p `Or` (q `And` r)) `Impl` ((p `Or` q) `And` (p `Or` r)))
test3 = tableaux

type x && y = And x y
infixr 3 &&

type x || y = Or x y
infixr 2 ||

type x --> y = Impl x y
infixr 1 -->

  
type x <-> y = (x --> y) && (y --> x)
infixr 1 <->
