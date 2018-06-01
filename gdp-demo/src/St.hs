{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module St
  ( St, Ref, runSt
  , newRef, readRef, writeRef
  , Common, runSt2, liftL, liftR
  , share, use, symm
  ) where

import Named

import Data.Coerce
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Store = Store

newtype Region n = Region Defn

-- Fake ST type; we'll just use IORefs behind the scenes.
newtype St n a = St (IO a) deriving (Functor, Applicative, Monad)

runSt :: (forall s. St s a) -> a
runSt (St action) = unsafePerformIO action

newtype Ref s a = Ref (IORef a)

newRef :: a -> St s (Ref s a)
newRef x = St (Ref <$> newIORef x)

readRef :: Ref s a -> St s a
readRef (Ref ref) = St (readIORef ref)

writeRef :: Ref s a -> a -> St s ()
writeRef (Ref ref) x = St (writeIORef ref x)

data Common s s'

share :: Ref s a -> St s (Ref (Common s s') a)
share = return . coerce

runSt2 :: (forall s s'. St (Common s s') a) -> a
runSt2 (St action) = unsafePerformIO action

liftL :: St s a -> St (Common s s') a
liftL = coerce

liftR :: St s' a -> St (Common s s') a
liftR = coerce

use :: Ref (Common s s') a -> Ref s a
use = coerce

symm :: Ref (Common s s') a -> Ref (Common s' s) a
symm = coerce

