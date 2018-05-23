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

type p && q = And p q
type p || q = Or p q
type p --> q = Impl p q

infixr 3 &&
infixr 2 ||
infixr 1 -->

data Proof p = QED deriving (Functor, Applicative, Monad)

factor_or :: ((p `Or` q) `And` (p `Or` r)) `Impl` (p `Or` (q `And` r))
factor_or = tableaux

factor_or :: (p || q) && (p || r) --> (p || (q && r))
factor_or = tableaux

factor_or :: (p || q) && (p || r) -> Proof (p || (q && r))
factor_or = f
