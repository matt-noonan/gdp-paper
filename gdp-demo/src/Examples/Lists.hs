{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Examples.Lists where

import           Prelude         hiding (head, reverse)
import qualified Prelude

import           Implicit
import           Named
import           Propositional
import           The

import           Data.Constraint
import           Data.Reflection

newtype IsCons xs = IsCons_ Defn
newtype IsNil  xs = IsNil_  Defn

data ListCases xs
  = IsCons (Proof (IsCons xs))
  | IsNil  (Proof (IsNil xs))

newtype Head xs = Head Defn
newtype Tail xs = Tail Defn

classifyList :: forall xs a. ([a] ~~ xs)
            -> Either (a ~~ Head xs, [a] ~~ Tail xs, Dict (Fact (IsCons xs)))
                      (Dict (Fact (IsNil  xs)))
classifyList (The xs) = case xs of
  (h:t) -> Left  (defn h, defn t, give (axiom @(IsCons xs)) Dict)
  []    -> Right (give (axiom @(IsNil xs)) Dict)

pattern Cons :: () => Fact (IsCons xs) => (a ~~ Head xs) -> ([a] ~~ Tail xs) -> ([a] ~~ xs)
pattern Cons h t <- (classifyList -> Left (h, t, Dict))

pattern Nil :: () => Fact (IsNil xs) => ([a] ~~ xs)
pattern Nil  <- (classifyList -> Right Dict)

-----

classify :: ([a] ~~ xs) -> ListCases xs
classify xs = case the xs of
  (_:_) -> IsCons axiom
  []    -> IsNil  axiom

newtype Reverse xs = Reverse Defn

reverse :: ([a] ~~ xs) -> ([a] ~~ Reverse xs)
reverse xs = defn (Prelude.reverse (the xs))

rev_cons :: Proof (IsCons xs) -> Proof (IsCons (Reverse xs))
rev_cons _ = axiom

head :: ([a] ~~ xs ::: IsCons xs) -> a
head xs = Prelude.head (the xs)

endpts :: IO (Int, Int)
endpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn :: IO [Int]
  name xs $ \xs -> case classify xs of
    IsCons isCons -> let ok = rev_cons isCons in
      return (head $ xs ...isCons, head (reverse xs ...ok))
    IsNil evidence  -> endpts

--------------------------------------------------------------
-- An alternate approach: passing the proof implicitly,
-- using Data.Reflection.

head' :: Fact (IsCons xs) => ([a] ~~ xs) -> a
head' (The xs) = Prelude.head xs

reverse :: ([a] ~~ xs) -> ([a] ~~ Reverse xs)
reverse (The xs) = defn (Prelude.reverse xs)

rev_cons :: Proof (IsCons xs) -> Proof (IsCons (Reverse xs))
rev_cons _ = axiom

endpts' :: IO (Int, Int)
endpts' = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  name xs $ \xs -> case xs of
    Cons _ _ -> noting (rev_cons `on` xs) $
      return (head' xs, head' (reverse xs))
    Nil      -> endpts'
