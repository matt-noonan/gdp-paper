module Named (Named, type (~~), name) where

import The

newtype Named name a = Named a
instance The (Named name a) a
type a ~~ name = Named name a

-- Morally, the type of `name` is
--      a -> (exists name. (a ~~ name))
name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)
