module Nats
  (
  ) where

newtype Nat n  :: Nat  Defn
newtype Zero   :: Zero Defn
newtype Succ n :: Succ Defn

succ :: Nat n -> Nat (Succ n)
succ _ = define

induction :: (p ::: Nat Zero) -> (forall n. p (Nat n) -> Proof (p (Nat (Succ n)))
                             
