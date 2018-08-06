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

on :: Fact p => (Proof p -> Proof q) -> (Fact q => t) -> t
on impl = give (impl given)

noting :: a => (a :- b) -> (b => r) -> r
noting impl k = k \\ impl

impl2sub :: forall p q. (Proof p -> Proof q) -> (Fact p :- Fact q)
impl2sub impl = Sub (give (impl (given :: Proof p)) Dict)

using :: Fact (p n) => (Proof (p n) -> Proof q) -> (a ~~ n) -> (Fact q => t) -> t
using impl _ = give (impl given)

note :: Proof p -> (Fact p => t) -> t
note = give
