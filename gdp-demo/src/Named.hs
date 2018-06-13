{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Named
  ( type (~~), name
  , Defn, Defining, defn, by_defn
  ) where

import The
import Data.Coerce

newtype Named name a = Named a
instance The (Named name a) a
type a ~~ name = Named name a

name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)

-----------------------------------------------------------

data Defn

type Defining t = (Coercible t Defn, Coercible Defn t)

defn :: Defining x => a -> (a ~~ x)
defn = coerce

by_defn :: forall p q. (Defining p, Defining q) => p -> q
by_defn = from_defn . to_defn
  where
    to_defn   = coerce :: p -> Defn
    from_defn = coerce :: Defn -> q
