{-# LANGUAGE RankNTypes #-}

module STMonadImpl where

import Data.IORef
import GHC.IO     (unsafePerformIO)

-- --------------------------------------------------------------------------------
-- Simple ST Monad implementation

newtype ST s a = ST
  { unsafeRunST :: a }

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure x = ST x
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
  return = pure
  ST a >>= k = seq a $ k a

-- Mutable variables can be introduced inside of the ST monad.
-- For our implementation, we can simply implement these in terms of
-- IORefs. We will wrap then in a newtype.

newtype STRef s a = STRef
  { unSTRef :: IORef a }

-- Function wrappers for STRef around IORef are provided, each of
-- which unsafely performs IO. For example, we'd like to be able
-- to create new STRefs.

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

-- A few more useful functions to wrap:

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef 

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref $ f a

-- Finally, we provide a function to escape from the ST monad.
-- This is merely unsafeRunST, but with a specialized type signature.

runST :: (forall s. ST s a) -- (*)
      -> a
runST = unsafeRunST 

-- (*) We see the introduction of the ST trick. The type
-- (forall s. ST s a) indicates that runST is capable of running only
-- those STs which do not depend on their 's' parameter.

-- We can write a safe usage of ST - one which uses its state to
-- compute a pure value.

safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ "world")
  readSTRef ref

safeExample' :: ST s Integer
safeExample' = do
  ref <- newSTRef 22
  modifySTRef ref (+ 2)
  readSTRef ref
