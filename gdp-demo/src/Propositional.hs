module Propositional
  ( And, Or, Not, Impl
  , or_elim, and_introL, and_introR, mp
  ) where

data Proof p = QED

data And p q
data Or p q
data Not p
data Impl p q

mp :: Impl p q -> p -> q
mp _ _ = error "mp"

or_elim :: (p -> r) -> (q -> r) -> (Or p q -> r)
or_elim _ _ = error "or_elim"

and_introL :: p -> And p q
and_introL _ = error "and_introL"

and_introR :: q -> And p q
and_introR _ = error "and_introR"
