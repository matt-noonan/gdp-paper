nonzero_length_implies_cons
  :: (Length xs == Succ n)
  -> Proof (IsCons xs)

nonzero_length_implies_cons eq =
  do  toSpec length
   |$ or_elimR and_elimL
   |/ and_elimR
   |. symmetric
   |. transitive' eq
   |. (contradicts' $$ zero_not_succ)
   |. absurd
