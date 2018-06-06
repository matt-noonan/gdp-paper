-- Unsafe API using non-total functions.
head :: [a] -> a
head xs = case xs of
  (x:_) -> x
  []    -> error "empty list!"

endpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  if xs /= [] then return (head xs, head $ reverse xs)
              else endpts
----------------------------------------------------------
-- Returning Maybe / Optional values. Safe, but requires
-- the caller to pattern-match on the Maybe at every use,
-- even when the list is known to be non-empty. Frustrated
-- users cannot be blamed for using `fromJust`!
headMay :: [a] -> Maybe a
headMay xs = case xs of
  (x:_) -> Just x
  []    -> Nothing

safeEndpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  case headMay xs of
    Just x -> return (x, fromJust (headMay $ reverse xs))
    _      -> safeEndpts
----------------------------------------------------------
-- "Ghosts of Departed Proofs". Safe. Does not return
-- an optional value; preconditions are checked early
-- and carried by "ghosts" (specialized phantom types).
rev_cons :: IsCons xs -> Proof (IsCons (Reverse xs))

gdpHead :: ([a] ~~ xs ::: IsCons xs) -> a
gdpHead xs = head (the xs) -- safe!

gdpEndpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  name xs $ \xs -> case classify xs of
    IsCons evidence -> let ok = evidence >>= rev_cons in
      return (gdpHead xs, gdpHead (gdpRev xs ...ok))
    IsNil  evidence -> gdpEndpts
