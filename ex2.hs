import Sized

dot :: List n Double -> List n Double -> Double
dot xs ys = sum (the $ sZipWith (*) xs ys)

main :: IO ()
main = do
  xs <- readLn
  ys <- readLn
  sizing xs $ \xs' -> do
    case align xs' ys of
      Nothing  -> putStrLn "Size mismatch!"
      Just ys' -> print (dot xs' ys')
