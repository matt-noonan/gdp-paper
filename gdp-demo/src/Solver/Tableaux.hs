{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Solver.Tableaux
  ( Formula(..)
  , sat
  , checkSat
  ) where

import qualified Data.Map as M
import System.IO.Unsafe

data Formula a
  = Atom a
  | Not (Formula a)
  | Impl (Formula a) (Formula a)
  | Or (Formula a) (Formula a)
  | And (Formula a) (Formula a)
  deriving Show

data FTree a
  = Leaf
  | Grow a (FTree a)
  | Fork (FTree a) (FTree a)
  deriving Show
    
tabl :: [(Bool, Formula a)] -> FTree (Bool, Formula a)
tabl [] = Leaf
tabl (x:ctx) = case x of
    (b, a@(Atom _))   -> Grow (b,a) (tabl ctx)
    (True,  Not p)    -> grow  ctx  (False, p)
    (False, Not p)    -> grow  ctx  (True,  p)
    (True, Or p q)    -> fork  ctx  (True,  p) (True,  q)
    (False, Or p q)   -> grow2 ctx  (False, p) (False, q)
    (True,  And p q)  -> grow2 ctx  (True,  p) (True,  q)
    (False, And p q)  -> fork  ctx  (False, p) (False, q)
    (True,  Impl p q) -> fork  ctx  (False, p) (True,  q)
    (False, Impl p q) -> grow2 ctx  (True,  p) (False, q)
  where
    grow  ctx x   = Grow x (tabl (x:ctx))
    grow2 ctx x y = Grow x (Grow y (tabl (x:y:ctx)))
    fork  ctx t f = Fork (grow ctx t) (grow ctx f)

compressTree :: FTree (Bool, Formula a) -> FTree (Bool, a)
compressTree = \case
      Grow (b, Atom a) t -> Grow (b,a) (compressTree t)
      Grow _ t    -> compressTree t
      Fork Leaf t -> compressTree t
      Fork t Leaf -> compressTree t
      Fork t t'   -> Fork (compressTree t) (compressTree t')
      Leaf        -> Leaf

satTree :: (Eq a, Ord a) => FTree (Bool, a) -> [M.Map a Bool]
satTree = go (M.fromList [])
  where
    go ctx = \case
      Leaf -> [ctx]
      Grow (b, x) t -> case M.lookup x ctx of
                        Just b' -> if b == b' then go ctx t else []
                        Nothing -> go (M.insert x b ctx) t
      Fork t t' -> go ctx t ++ go ctx t'

sat :: (Eq a, Ord a, Show a) => Formula a -> Bool
sat p = unsafePerformIO $ do
  print p
  let ans = (satTree . compressTree . tabl) [(False, p)]
  case ans of
    [] -> return True
    (x:_) -> do
        putStrLn ("UNSAT: " ++ show (M.toList x))
        return False
--  return $ ((== []) . satTree . compressTree . tabl) [(False, p)]

data SAT a = Sat | Unsat [(a, Bool)] deriving Show

checkSat :: (Eq a, Ord a) => Formula a -> SAT a
checkSat p = case (satTree $ compressTree $ tabl [(False, p)]) of
  (m:_) -> Unsat (M.toList m)
  []    -> Sat
