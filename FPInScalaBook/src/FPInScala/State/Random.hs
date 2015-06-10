module FPInScala.State.Random (
    RNG,
    Random,
    SimpleRNG(..),
    PositiveInt,
    ints,
    ) where

import Data.Word (Word64)
import Data.Bits (shiftL, shiftR, xor, (.&.))

-- There are a couple ways to do this:
--  Follow something like System.Random
--   and define sources of randomness and things
--   to take randomness and produce variables
--  A different method is to work with something
--   more like the State monad
--  The former is probably easier to understand, the
--   latter allows more combination, etc.

-- First: lets take the System.Random approach:
-- Definine random number generators in terms of
-- their ability to make random integers
-- See System.Random for a more serious implementation
class RNG a where
    nextInt :: a -> (Int, a)

-- Something that can be combined with an RNG to get a random value
class Random a where
  generate :: RNG g => g -> (a, g)
  getRange :: (a, a)

-- Simple LCG RNG
data SimpleRNG = SimpleRNG Word64

instance RNG SimpleRNG where
    nextInt (SimpleRNG s) = (n, SimpleRNG newSeed)
        where m = 0x5DEECE66D :: Word64
              b = 0xB :: Word64
              newSeed = m * s + b
              n = fromIntegral $ shiftR newSeed 16

-- Here's a more serious random number generator
--  from Numerical Recipes 3rd edition
data NumRecRNG = NumRecRNG (Word64, Word64, Word64)

int64 :: NumRecRNG -> (Word64, NumRecRNG)
int64 (NumRecRNG (u, v, w)) = let umult = 2862933555777941757 :: Word64
                                  uplus = 7046029254386353087 :: Word64
                                  unew = umult * u + uplus
                                  v1 = v `xor` (shiftR v 17)
                                  v2 = v1 `xor` (shiftL v1 31)
                                  vnew = v2 `xor` (shiftR v2 8)
                                  wmult = 4294957665 :: Word64
                                  wand =  0xffffffff :: Word64
                                  wnew = wmult * (w .&. wand) + (shiftR w 32)
                                  x1 = unew `xor` (shiftL unew 21)
                                  x2 = x1 `xor` (shiftR x1 35)
                                  x3 = x2 `xor` (shiftL x2 4)
                                  x = (x + vnew) `xor` wnew
                              in (x, NumRecRNG (unew, vnew, wnew))


-- Smart constructor from seed for NumRecRNG
numRecRNG :: Word64 -> NumRecRNG
numRecRNG seed = let v = 4101842887655102017 :: Word64
                     w = 1 :: Word64
                     u = seed `xor` v
                     gen1 = NumRecRNG (u, v, w)
                     NumRecRNG (u2, v2, w2) = snd $ int64 gen1
                     gen2 = NumRecRNG (u2, u2, w2)
                     NumRecRNG (u3, v3, w3) = snd $ int64 gen2
                     gen3 = NumRecRNG (u3, v3, v3)
                 in snd $ int64 gen3

instance RNG NumRecRNG where
  nextInt r = let (n, newr) = int64 r
              in (fromIntegral n :: Int, newr)

-- Now instances of Random
instance Random Int where
  generate = nextInt
  getRange = (minBound :: Int, maxBound :: Int)

newtype PositiveInt = PositiveInt Int
instance Random PositiveInt where
  generate r = let (v, newr) = nextInt r
                   val = if (v < 0) then (-(v+1)) else v
                in (PositiveInt val, newr)
  getRange = (PositiveInt 0, PositiveInt (maxBound :: Int))

instance Random Bool where
  generate r = (v <= 0, newr)
    where (v, newr) = nextInt r
  getRange = (False, True)

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

