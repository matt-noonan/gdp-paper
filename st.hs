stSharingDemo :: Bool
stSharingDemo = runSt2 $ do
  -- In the "left" memory region, create and return
  -- two references; one shared, and one not shared.
  (secret, ref) <- liftL $ do
      unshared <- newRef 42
      shared   <- share =<< newRef 17
      return (unshared, shared)
  -- In the "right" memory region, mutate the shared
  -- reference. If we attempt to access the non-shared
  -- reference here, the program will not compile.
  liftR $ do
      let mine = use (symm ref)
      x <- readRef mine
      writeRef mine (x + 1)
  -- Back in the "left" memory region, verify that the
  -- unshared reference still holds its original value.
  liftL $ do
      check <- readRef secret
      return (check == 42)
