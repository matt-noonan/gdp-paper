-- Unsafe API using non-total functions.
head :: [a] -> a
head xs = case xs of
  (x:_) -> x
  []    -> error "empty list!"

endpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  return (head xs, head $ reverse xs)

--------------------------------------------------------
-- Returning Maybe / Optional values. Safe, but requires
-- the caller to pattern-match on the Maybe at every use,
-- even when the list is known to be non-empty. Frustrated
-- users cannot be blamed for using `fromJust`!
safeHead :: [a] -> Maybe a
safeHead xs = case xs of
  (x:_) -> Just x
  []    -> Nothing

safeEndpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  case safeHead xs of
    Just x -> return (x, fromJust (safeHead $ reverse xs))
    _      -> safeEndpts

-------------------------------------------------------
-- "Ghosts of Departed Proofs". Safe. Does not return
-- an optional value; preconditions are checked early
-- and carried by "ghosts" (specialized phantom types).
rev_cons :: IsCons xs -> Proof (IsCons (Reverse xs))

gdpHead :: (a ~~ xs ::: IsCons xs) -> a
gdpHead xs = head (the xs)

gdpEndpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  name xs $ \xs -> case classify xs of
    Right is_cons -> let ok = is_cons |$ rev_cons in
      return (gdpHead xs, gdpHead (gdpRev xs ...ok))
    Left  _  -> gdpEndpts
