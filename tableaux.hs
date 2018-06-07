type Theorem p q = (p --> q) --> (Not q --> Not p)
proof1, proof2 :: Proof (Theorem p q)

proof1 =
  impl_intro $ \p_implies_q ->
    impl_intro $ \not_q ->
      not_intro $ \p -> do
        q <- impl_elim p_imples_q p
        contradicts q not_q

proof2 = tableaux
