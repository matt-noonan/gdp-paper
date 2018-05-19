test_table = Map.fromList [ (1, "Hello")
                          , (2, "world!") ]

withMap test_table $ \table ->
  case member 1 table of

    Nothing  -> putStrLn "Missing key!"

    Just key -> do
      putStrLn ("Found key: " ++ show (the key))
      putStrLn ("Value in map 1: " ++
                lookup key table)

      let table'  = reinsert key "Howdy" table
          table'' = fmap (map upper) table
      putStrLn ("Value in map 2: " ++
                lookup key table')
      putStrLn ("Value in map 3: " ++
                lookup key table'')
{- Output:
Found key: 1
Value in map 1: Hello
Value in map 2: Howdy
Value in map 3: HELLO
-}
