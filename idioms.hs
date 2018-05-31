--------------------------------------------------------
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

--------------------------------------------------------
-- Returning Maybe / Optional values. Safe, but requires
-- the caller to pattern-match on the Maybe at every use,
-- even when the list is known to be non-empty.

safeHead :: [a] -> Maybe a
safeHead xs = case xs of
  (x:_) -> Just x
  []    -> Nothing

safeEndpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  case (safeHead xs, safeHead $ reverse xs) of
    (Just x, Just y) -> return (x, y)
     _               -> safeEndpts

-------------------------------------------------------
-- "Ghosts of Departed Proofs". Safe. Does not return
-- an optional value; preconditions are checked early
-- and represented in phantom type parameters.

gdpHead :: List NonZero a -> a
gdpHead xs = case the xs of
  (x:_) -> x
  []    -> unreachable

gdpEndpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  case classify xs of
    Right xs -> return (gdpHead xs, gdpHead $ gdpRev xs)
    Left  _  -> gdpEndpts
