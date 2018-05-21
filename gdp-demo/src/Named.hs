{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}

module Named
  ( type (~~), name
  , Defn, Defining, define
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

define :: Defining x => a -> (a ~~ x)
define = coerce
