import Sorted
import Named
main = do
  xs <- readLn :: IO Int
  ys <- readLn
  name (comparing Down) $ \gt -> do
    let xs' = sortBy gt xs
        ys' = sortBy gt ys
    print (the (mergeBy gt xs' ys'))
