test = Map.fromList [ (1, "Hello"), (2, "world!") ]

name test $ \table -> name 1 $ \k ->
  case member k table of

    Nothing  -> putStrLn "Missing key!"

    Just key -> do
      let table'  = reinsert key "Howdy" table
          table'' = fmap (map upper) table
      putStrLn ("Value in map 1: " ++ lookup key table)
      putStrLn ("Value in map 2: " ++ lookup key table')
      putStrLn ("Value in map 3: " ++ lookup key table'')
-- Output:
--   Value in map 1: Hello
--   Value in map 2: Howdy
--   Value in map 3: HELLO
