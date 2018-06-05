runSt2 :: (forall s s'. St (s #$\cap$# s') a) -> a

liftL :: St s a -> St (s #$\cap$# s') a
liftR :: St s' a -> St (s #$\cap$# s') a

share :: (a #$\in$# Region s) -> St s (a #$\in$# Region (s #$\cap$# s'))

use  :: (a #$\in$# Region (s #$\cap$# s')) -> (a #$\in$# Region s)
symm :: (a #$\in$# Region (s #$\cap$# s')) -> (a #$\in$# Region (s' #$\cap$# s))
