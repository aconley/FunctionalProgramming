module FPInScala.State (
    RNG,
    SimpleRNG(..),
    nonNegativeInt,
    getUniformDouble,
    ints,
    State(..)
    ) where

import Data.Word (Word64)
import Data.Bits (shiftR)
import Control.Applicative

-- Definine random number generators in terms of
-- their ability to make random integers
class RNG a where
    nextInt :: a -> (Int, a)

-- This will be our RandomState generator
data SimpleRNG = SimpleRNG Word64

instance RNG SimpleRNG where
    nextInt (SimpleRNG s) = (n, SimpleRNG newSeed)
        where m = 0x5DEECE66D :: Word64
              b = 0xB :: Word64
              newSeed = m * s + b
              n = fromIntegral $ shiftR newSeed 16

-- Generate a positive Integer
nonNegativeInt :: RNG a => a -> (Int, a)
nonNegativeInt r = if (v < 0) then (-(v+1), newr) else (v, newr)
    where (v, newr) = nextInt r

-- Generate a double in the range [0, 1)
getUniformDouble :: RNG a => a -> (Double, a)
getUniformDouble r = (v / maxval, newr)
    where (vint, newr) = nextInt r
          v = fromIntegral vint :: Double
          maxval = (fromIntegral (maxBound :: Int) :: Double) + 1.0

-- Generate a list of random ints
ints :: RNG a => a -> Int -> ([Int], a)
ints r n = f r n []
  where f rnd m xs | m <= 0 = (xs, rnd)
                   | otherwise = f newrnd (m - 1) (x : xs)
           where (x, newrnd) = nextInt rnd

-- Full fledged State.  Note: Haskell already has Control.Monad.State
data State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State h) = State $ \s -> let (a, newState) = h s
                                     in (f a, newState)

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s
                                      (State g) = f a
                                  in g newState

instance Applicative (State s) where
  pure = return
  f <*> x = do
    f' <- f
    x' <- x
    return (f' x')
