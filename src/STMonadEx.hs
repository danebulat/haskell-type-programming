{-# LANGUAGE RankNTypes #-}

module STMonadEx where

import Control.Monad.ST ( ST, runST ) 
import Control.Monad    ( forM_ )
import Data.STRef       ( modifySTRef, newSTRef, readSTRef, writeSTRef, STRef )
import Data.Foldable    ( for_ )

import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V

-- --------------------------------------------------------------------------------
-- Sum example 

-- runST takes our stateful code and makes it pure again
sumST :: Num a => [a] -> a
sumST xs = runST $ do
  -- Create a STRef (place in memory to store values)
  n <- newSTRef 0

  -- For each element of xs, add it to n
  forM_ xs $ \x -> do
    modifySTRef n (+x)

  -- Read the value of n, and return it
  readSTRef n

-- --------------------------------------------------------------------------------
-- Implementation of foldl using ST

foldlST :: (a -> b -> a) -> a -> [b] -> a
foldlST f acc xs = runST $ do
  -- Create a variable for the accumulator
  acc' <- newSTRef acc

  -- For each x in xs, update the accumulator by calling the fold function 
  forM_ xs $ \x -> do
    a <- readSTRef acc'
    writeSTRef acc' (f a x)

  -- Read the result
  readSTRef acc'

-- --------------------------------------------------------------------------------
-- Fibonacci function running in constant space 

fibST :: Integer -> Integer
fibST n
  | n < 2 = n
  | otherwise = runST $ do
      x <- newSTRef 0
      y <- newSTRef 1
      fibST' n x y
  where
    fibST' 0 x _ = readSTRef x
    fibST' n x y = do
      x' <- readSTRef x
      y' <- readSTRef y
      writeSTRef x y'
      writeSTRef y $! x' + y'
      fibST' (n-1) x y

-- --------------------------------------------------------------------------------
-- Another sum example

-- A detail worth noting is that even though for_ folds the list from
-- the right the sums are done from the left, as the mutations are
-- performed as applicative effects sequenced from left to right.

sumST' :: Num a => [a] -> a
sumST' xs = runST $ do
  n <- newSTRef 0
  for_ xs $ \x ->
    modifySTRef n (+x)
  readSTRef n

-- --------------------------------------------------------------------------------
-- Array examples

makeArray :: [Integer]
makeArray = runST $ do
  n <- newSTRef [1,2,3]
  readSTRef n

makeArray' :: ST s Integer
makeArray' = newSTRef 10 >>= readSTRef

makeArray'' :: ST s (STRef s Integer, STRef s Integer)
makeArray'' = do
  a <- newSTRef 10
  b <- newSTRef 11
  return (a, b)

makeVec :: V.Vector Double
makeVec = runST $ do
  v <- M.replicate 3 (1.2 :: Double)
  M.write v 1 3.1
  V.freeze v

