{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ViewPatterns           #-}

module The (The(..), pattern The) where

import           Data.Coerce

class The d a | d -> a where
  the :: d -> a
  default the :: Coercible d a => d -> a
  the = coerce

{- You can use this pattern synonym on the left-hand
   side of a definition to replace uses of `the` on
   the right-hand side, as in:

> -- `the` on RHS
> foo x = bar (the x) + baz (the x)

> -- `The` on LHS
> foo (The x) = bar x + baz x
-}

pattern The :: The d a => a -> d
pattern The x <- (the -> x)
