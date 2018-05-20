{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}

module Named
  ( Named, type (~~), name
  ) where

import The
import Data.Coerce

newtype Named name a = Named a
instance The (Named name a) a
type a ~~ name = Named name a

name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)
