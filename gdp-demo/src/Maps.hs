{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Maps
  ( Keys, Elt, withMap, member, lookup, reinsert
  ) where

import Prelude hiding (lookup)

import The
import Named

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Coerce

newtype Keys s = Keys Defn

newtype Elt s = Elt Defn

newtype MapK s k v = MapK (Map k v ~~ Keys s)

type k ∈ s = k ~~ Elt s

instance The (MapK s k v) (Map k v) where
  the (MapK m) = the m
 
instance Functor (MapK s k) where
  fmap f = MapK . defn . fmap f . the

withMap :: Map k v -> (forall s. MapK s k v -> t) -> t
withMap m k = k (MapK $ defn m)

member :: Ord k => k -> MapK s k v -> Maybe (k ∈ s)
member k m = case M.lookup k (the m) of
  Nothing -> Nothing
  Just _  -> Just (defn k)

lookup :: Ord k => (k ∈ s) -> (MapK s k v) -> v
lookup k m = case M.lookup (the k) (the m) of
  Nothing -> error "unreachable"
  Just v  -> v
  
reinsert :: Ord k => (k ∈ s) -> v -> MapK s k v -> MapK s k v
reinsert k v m = MapK (defn (M.insert (the k) v (the m)))

