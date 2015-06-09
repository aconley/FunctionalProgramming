module FPInScala.State.Random (
    RNG,
    Random,
    SimpleRNG(..),
    PositiveInt,
    ints,
    ) where

import Data.Word (Word64)
import Data.Bits (shiftR)
-- Definine random number generators in terms of
-- their ability to make random integers
-- See System.Random for a more serious implementation
class RNG a where
    nextInt :: a -> (Int, a)

-- Something that can be combined with an RNG to get a random value
class Random a where
  generate :: RNG g => g -> (a, g)
  getRange :: (a, a)

-- This will be our RandomState generator that uses
--  a 64 bit unsigned value for internal state
data SimpleRNG = SimpleRNG Word64

instance RNG SimpleRNG where
    nextInt (SimpleRNG s) = (n, SimpleRNG newSeed)
        where m = 0x5DEECE66D :: Word64
              b = 0xB :: Word64
              newSeed = m * s + b
              n = fromIntegral $ shiftR newSeed 16

instance Random Int where
  generate = nextInt
  getRange = (minBound :: Int, maxBound :: Int)

newtype PositiveInt = PositiveInt Int
instance Random PositiveInt where
  generate r = let (v, newr) = nextInt r
                   val = if (v < 0) then (-(v+1)) else v
                in (PositiveInt val, newr)
  getRange = (PositiveInt 0, PositiveInt (maxBound :: Int))

instance Random Double where
  generate r = (v / maxval, newr)
    where (vint, newr) = nextInt r
          v = fromIntegral vint :: Double
          maxval = (fromIntegral (maxBound :: Int) :: Double) + 1.0
  getRange = (0, 1)

-- Generate a list of random ints
ints :: RNG a => a -> Int -> ([Int], a)
ints r n = f r n []
  where f rnd m xs | m <= 0 = (xs, rnd)
                   | otherwise = f newrnd (m - 1) (x : xs)
           where (x, newrnd) = nextInt rnd

