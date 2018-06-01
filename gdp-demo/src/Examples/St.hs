module Examples.St where

import St

test1 :: (Int, Bool)
test1 = runSt2 $ do
  (secret, ref) <- liftL (new 1)
  liftR (inc ref)
  ok <- liftL (check secret ref)
  x  <- readRef ref
  return (x, ok)

new :: Int -> St s (Ref s Int, Ref (Common s s') Int)
new x = do
  secret <- newRef x
  ref    <- newRef x
  shared <- share ref
  return (secret, shared)

inc :: Ref (Common s s') Int -> St s' ()
inc ref = do
  let mine = use (symm ref)
  x <- readRef mine
  writeRef mine (x + 1)

check :: Ref s Int -> Ref (Common s s') Int -> St s Bool
check secret ref = do
  original <- readRef secret
  actual   <- readRef (use ref)
  return (original == actual)
  
test2 :: (Int, Bool)
test2 = runSt2 $ do
  (secret, ref) <- liftL (new 1)
  liftR (inc' ref secret)
  ok <- liftL (check secret ref)
  x  <- readRef ref
  return (x, ok)

inc' :: Ref (Common s s') Int -> Ref s Int -> St s' ()
inc' ref secret = do
  let mine = use (symm ref)
  x <- readRef mine
  writeRef mine (x + 1)
  -- If we try to modify the secret, comilation fails;
  -- the secret doesn't belong to the common memory
  -- region for s and s'.
  --writeRef secret (x + 1)

