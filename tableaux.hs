proof1, proof2 :: Proof (p {$\wedge$} q --> p {$\vee$} q)

proof1 =  and_elimL
       |. or_introL
       |\ impl_intro

proof2 = tableaux
