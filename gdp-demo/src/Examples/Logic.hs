{-# OPTIONS_GHC -fplugin=Tableaux #-}
{-# LANGUAGE TypeOperators        #-}

module Examples.Logic where

import Tableaux
import Propositional


test :: And p q `Impl` Or p q
test = tableaux

test2 :: And p q -> Or p q
test2 = mp tableaux

-- Distributivity of Or over And
test3 :: ( ((p `Or` q) `And` (p `Or` r)) `Impl` (p `Or` (q `And` r))) `And` ((p `Or` (q `And` r)) `Impl` ((p `Or` q) `And` (p `Or` r)))
test3 = tableaux

type Contrapositive p q = (p `Impl` q) `Impl` (Not q `Impl` Not p)

-- Tableau example
test4 :: Contrapositive p q
test4 = tableaux
