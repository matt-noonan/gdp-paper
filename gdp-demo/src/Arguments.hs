{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Arguments
  ( Arg(..)
  , arg
  , Argument(..)
  ) where

import GHC.TypeLits

data Arg n = Arg

arg :: Arg n
arg = Arg

class Argument (f :: *) (n :: Nat) where
  type GetArg f n   :: *
  type SetArg f n x :: *
