{-# LANGUAGE TypeOperators #-}
module GdpList
    ( reverse
    , length
    , zipWith
    , Length
    , Rev
    , rev_length
    , rev_rev
    , rev_cons
    , ListCase (..)
    , classify
    ) where

import           Named
import           Prelude       hiding (length, reverse, zipWith)
import qualified Prelude
import           Propositional
import           The

-- API functions
reverse :: ([a] ~~ xs) -> ([a] ~~ Rev xs)
reverse xs = defn (Prelude.reverse (the xs))

length :: ([a] ~~ xs) -> (Int ~~ Length xs)
length xs = defn (Prelude.length (the xs))

zipWith :: (a -> b -> c)
         -> ([a] ~~ xs ::: Length xs == n)
         -> ([b] ~~ ys ::: Length ys == n)
         -> [c]
zipWith f xs ys = Prelude.zipWith f (the xs) (the ys)

-- Names for API functions
newtype Length xs = Length  Defn
newtype Rev    xs = Rev     Defn

-- Lemmas
rev_length :: Proof (Length (Rev xs) == Length xs)
rev_length = axiom

rev_rev    :: Proof (Rev (Rev xs) == xs)
rev_rev = axiom

rev_cons   :: Proof (IsCons xs) -> Proof (IsCons (Rev xs))
rev_cons _ = axiom

data IsCons xs
data IsNil  xs

data ListCase a xs = IsCons (Proof (IsCons xs))
                   | IsNil  (Proof (IsNil  xs))

classify :: ([a] ~~ xs) -> ListCase a xs
classify xs = case the xs of
  (_:_) -> IsCons axiom
  []    -> IsNil  axiom
