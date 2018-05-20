module Examples.Dot where

import Sized

dot :: Size n [Double] -> Size n [Double] -> Double
dot xs ys = sum (the $ sZipWith (*) xs ys)

dot_demo :: IO ()
dot_demo = do
  xs <- readLn
  ys <- readLn
  sizing xs $ \xs' -> do
    case align xs' ys of
      Nothing  -> putStrLn "Size mismatch!"
      Just ys' -> print (dot xs' ys')

---------------------------------------------------------------------

norm2 :: Size n [Double] -> Double
norm2 xs = dot xs (ok $ xs +++ nil)
  where
    ok = zero_neutral . add_commutes
    
---------------------------------------------------------------------

pushMany :: [a] -> Size Infinite [a] -> Size Infinite [a]
pushMany xs stack = sizing xs (\v -> ok $ v +++ stack)
  where
    ok = infinity_absorbs
