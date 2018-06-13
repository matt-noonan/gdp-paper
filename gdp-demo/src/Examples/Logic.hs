{-# OPTIONS_GHC -fplugin=Tableaux #-}
{-# LANGUAGE TypeOperators        #-}

module Examples.Logic where

import Tableaux
import Propositional


test :: Proof (p && q --> p || q)
test = tableaux

test2 :: p && q -> Proof (p || q)
test2 = modus_ponens tableaux'

-- Distributivity of Or over And
test3 :: Proof (( ((p || q) && (p || r)) --> (p || (q && r))) && ((p || (q && r)) --> ((p || q) && (p || r))))
test3 = tableaux

type Contrapositive p q = Proof ( (p --> q) --> (Not q --> Not p))

-- Tableau example
test4 :: Contrapositive p q
test4 = tableaux
