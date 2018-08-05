{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Implicit (ok, using, noting, impl2sub, Fact, note, on) where

import           Named
import           Propositional
import           The

import           Data.Constraint
import           Data.Reflection

type Fact p = Given (Proof p)

ok :: Fact p => a -> (a ::: p)
ok = (...given)

using :: Fact p => (Proof p -> Proof q) -> (Fact q => t) -> t
using impl = give (impl given)

noting :: a => (a :- b) -> (b => r) -> r
noting impl k = k \\ impl

impl2sub :: forall p q. (Proof p -> Proof q) -> (Fact p :- Fact q)
impl2sub impl = Sub (give (impl (given :: Proof p)) Dict)

on :: (Proof (p n) -> Proof q) -> (a ~~ n) -> (Fact (p n) :- Fact q)
on impl _ = impl2sub impl

note :: Proof p -> (Fact p => t) -> t
note = give
