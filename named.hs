module Named (Named, name) where

import The

newtype Named name a = Named a
instance The (Named name a) a

name :: a -> (forall name. Named name a -> t) -> t
name x k = k (coerce x)
