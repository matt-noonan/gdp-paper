runSt    :: (forall s. St s a) -> a

newRef   :: a -> St s (a #$\in$# Region s)
readRef  :: (a #$\in$# Region s) -> St s a
writeRef :: (a #$\in$# Region s) -> a -> St s ()
