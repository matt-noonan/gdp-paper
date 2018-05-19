{-# LANGUAGE RankNTypes #-}

import Data.Coerce (coerce)
  
newtype Size n a = Size a

the :: Size n a -> a
the = coerce

sZipWith :: (a -> b -> c)
         -> Size n [a]
         -> Size n [b]
         -> Size n [c]
sZipWith f xs ys =
  coerce (zipWith f (the xs) (the ys))
  
sizing :: [a] -> (forall n. Size n [a] -> t) -> t
sizing xs k = k (coerce xs)

align :: Size n [a] -> [b] -> Maybe (Size n [b])
align xs ys = if length (the xs) == length ys
              then Just (coerce ys)
              else Nothing


dot :: Size n [Double] -> Size n [Double] -> Double
dot xs ys = sum (the $ sZipWith (*) xs ys)

main' :: IO ()
main' = do
  xs <- readLn
  ys <- readLn
  sizing xs $ \xs' -> do
    case align xs' ys of
      Nothing  -> putStrLn "Size mismatch!"
      Just ys' -> print (dot xs' ys')
