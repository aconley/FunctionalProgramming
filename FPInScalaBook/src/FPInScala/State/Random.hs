module FPInScala.State.Random (
    RNG,
    nextInt,
    Random,
    generate,
    getRange,
    SimpleRNG(..),
    NumRecRNG(..),
    numRecRNG,
    PositiveInt,
    ints,
    Rand(..),
    randInt,
    unit,
    rMap,
    nonNegativeInt,
    nonNegativeIntEven,
    randDouble,
    rMap2,
    both,
    sequenceR,
    flatMap,
    nonNegativeLessThan
    ) where

import Data.Word (Word64)
import Data.Int (Int64)
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
data SimpleRNG = SimpleRNG Int64 deriving (Eq, Show)

instance RNG SimpleRNG where
    nextInt (SimpleRNG s) = (n, SimpleRNG newSeed)
        where m = 0x5DEECE66D :: Int64
              b = 0xB :: Int64
              newSeed = m * s + b
              n = fromIntegral $ shiftR newSeed 16

-- Here's a more serious random number generator
--  from Numerical Recipes 3rd edition
data NumRecRNG = NumRecRNG (Word64, Word64, Word64) deriving (Eq, Show)

word64 :: NumRecRNG -> (Word64, NumRecRNG)
word64 (NumRecRNG (u, v, w)) = let umult = 2862933555777941757 :: Word64
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
                                   x = (x3 + vnew) `xor` wnew
                               in (x, NumRecRNG (unew, vnew, wnew))


-- Smart constructor from seed for NumRecRNG
numRecRNG :: Word64 -> NumRecRNG
numRecRNG seed = let v = 4101842887655102017 :: Word64
                     w = 1 :: Word64
                     u = seed `xor` v
                     gen1 = NumRecRNG (u, v, w)
                     NumRecRNG (u2, _, w2) = snd $ word64 gen1
                     gen2 = NumRecRNG (u2, u2, w2)
                     NumRecRNG (u3, v3, _) = snd $ word64 gen2
                     gen3 = NumRecRNG (u3, v3, v3)
                 in snd $ word64 gen3

instance RNG NumRecRNG where
  nextInt r = let (n, newr) = word64 r
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

-- Alternative approach: much more state like
--  generate is a function that takes a RNG
--  and uses it to produce a random number and
--  new RNG
data Rand r a = Rand { generateR :: r -> (a, r)}

unit :: (RNG r) => a -> Rand r a
unit v = Rand $ \rng -> (v, rng)

randInt :: Rand SimpleRNG Int
randInt = Rand nextInt

rMap :: (a -> b) -> Rand r a -> Rand r b
rMap g (Rand f) = Rand $ \rng -> let (v1, rng2) = f rng
                                 in (g v1, rng2)

rMap2 :: (a -> b -> c) -> Rand r a -> Rand r b -> Rand r c
rMap2 f (Rand g) (Rand h) = Rand $ \rng -> let (va, rng2) = g rng
                                               (vb, rng3) = h rng2
                                           in (f va vb, rng3)

nonNegativeInt :: Rand SimpleRNG Int
nonNegativeInt = rMap p randInt
  where p v = if (v < 0) then (-(v+1)) else v

nonNegativeIntEven :: Rand SimpleRNG Int
nonNegativeIntEven = rMap g nonNegativeInt
    where g i = i - i `mod` 2

randDouble :: Rand SimpleRNG Double
randDouble = rMap d randInt
  where d v = let maxval = (fromIntegral (maxBound :: Int) :: Double) + 1.0
              in (fromIntegral v :: Double) / maxval

both :: Rand r a -> Rand r b -> Rand r (a, b)
both = rMap2 (\va vb -> (va, vb))

sequenceR :: [Rand r a] -> Rand r [a]
sequenceR fs = Rand $ \rng -> sequenceR' fs rng []
  where sequenceR' [] rgen acc = (acc, rgen)
        sequenceR' (x:xs) rgen acc = let (newval, newr) = generateR x $ rgen
                                     in sequenceR' xs newr (newval : acc)

flatMap :: Rand r a -> (a -> Rand r b) -> Rand r b
flatMap (Rand f) g = Rand $ \rng -> let (va, rng2) = f rng
                                    in (generateR $ g va) rng2

nonNegativeLessThan :: Int -> Rand SimpleRNG Int
nonNegativeLessThan 0 = unit 0
nonNegativeLessThan n | n < 0 = error "Can't have negative n"
nonNegativeLessThan n = flatMap nonNegativeInt f
                        where f i = let m = i `mod` n
                                    in if ((i + (n - 1) - m) >= 0) then unit m
                                       else nonNegativeLessThan n
