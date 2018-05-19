import Sorted
import Named
main = do
  xs <- readLn :: IO Int
  ys <- readLn
  name (>) $ \gt -> do
    let xs' = sortBy gt xs
        ys' = sortBy gt ys
    print (the xs', the ys', the (mergeBy gt xs' ys'))
