runSt    :: (forall s. St s a) -> a

newRef   :: a -> St s (a #$\in$# s)
readRef  :: (a #$\in$# s) -> St s a
writeRef :: (a #$\in$# s) -> a -> St s ()
