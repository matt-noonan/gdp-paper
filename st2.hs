runSt2 :: (forall s s'. St (s #$\cap$# s') a) -> a

liftL :: St s a -> St (s #$\cap$# s') a
liftR :: St s' a -> St (s #$\cap$# s') a

share :: (a #$\in$# s) -> St s (a #$\in$# (s #$\cap$# s'))

use  :: (a #$\in$# (s #$\cap$# s')) -> (a #$\in$# s)
symm :: (a #$\in$# (s #$\cap$# s')) -> (a #$\in$# (s' #$\cap$# s))
