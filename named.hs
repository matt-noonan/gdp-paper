module Named
  (Named, type (~~), name, Defn, defn, Defining) where

import The
import Data.Coerce

newtype Named name a = Named a
instance The (Named name a) a
type a ~~ name = Named name a

-- Morally, the type of `name` is
--      a -> (exists name. (a ~~ name))
name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)

data Defn = Defn
type Defining f = (Coercible f Defn, Coercible Defn f)

-- Allow library authors to introduce their own names.
defn :: Defining f => a -> (a ~~ f)
defn = coerce
