module FPInScala.State (
    RNG,
    SimpleRNG,
    nonNegativeInt,
    getUniformDouble
    ) where

import Data.Word (Word64)
import Data.Bits (shiftR)

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
