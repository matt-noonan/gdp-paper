{- "Ghosts on the outside, Liquid Haskell on the inside"

This file shows how to export an interface using GDP, but have
the module's internals checked by LiquidHaskell. To check the
internals, follow these steps:

  1) Install an SMT solver, such as Z3, CVC4, or MathSAT.

  2) Un-comment the extra-deps in stack.yaml to add the dependencies
     for LiquidHaskell.

  3) From the gdp-demo directory run

        stack install liquidhaskell

  4) Change to the src directory, then check the internal
     implementation by running

        liquid Examples/Liquid/Internal.hs

  5) You should see an error, where LiquidHaskell has determined
     that the implementation in Examples.Liquid.Internal did not
     meet the spec. That means that *this* module has a bug:
     the GDP types expressed here are not all correct!

  6) Fix the bug and profit!

-}

{-# LANGUAGE TypeOperators #-}

module Examples.Liquid where

import           Named
import           Propositional
import           The

import qualified Examples.Liquid.Internal as LH

type Even = Int ? IsEven -- (a ? p) is equivalent to (a ~~ name ::: p name)

newtype IsEven n = Even Defn

-----------------------------------------------------------------------
-- `weAreHalves` and `someHalf` should not cause run-time errors
-----------------------------------------------------------------------

half :: Even -> Int
half x = case the x `divMod` 2 of
  (n, 0) -> n
  (_, 1) -> error "impossible, if LiquidHaskell accepts Examples.Liquid.Internal!"

weAreHalves :: [Int]
weAreHalves = map half weAreEven

someHalf :: Int
someHalf = half someEven

-----------------------------------------------------------------------
-- Wrap the Examples.Liquid.Internal API with GDP types.
--
-- NOTE: The GDP types here MUST match the corresponding
--       LiquidHaskell annotations in Examples.Liquid.Internal!
--       The implementations should do nothing except strip
--       GDP types from arguments (e.g. using `the`) and
--       introduce GDP refinements on the results (e.g. using
--       `assert`). The real work should be delegated to the
--       LiquidHaskell-checked internal implementation.
-----------------------------------------------------------------------

weAreEven :: [Even]
weAreEven = map assert LH.weAreEven

someEven :: Even
someEven = assert LH.someEven

evens :: Int -> [Even]
evens = map assert . LH.evens

shift :: [Even] -> Even -> [Even]
shift xs k = map assert (LH.shift (map the xs) (the k))

double :: [Int] -> [Even]
double = map assert . LH.double

