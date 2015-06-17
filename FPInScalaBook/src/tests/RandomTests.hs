module RandomTests (random_tests) where

import Test.HUnit
import FPInScala.State.Random
import Data.Word (Word64)

test_ints = TestCase $ do
    assertEqual "ints should return an empty list when n = 0"
        [] (fst $ ints (SimpleRNG 0x9384ABF20) 0)
    assertEqual "ints should return an empty list when n < 0"
        [] (fst $ ints (SimpleRNG 0x9384ABF20) (-3))
    assertEqual "ints should return an 5 element when n = 5"
        (5) (length . fst $ ints (SimpleRNG 0x9384ABF20) 5)

test_simple = TestCase $ do
    let rng = SimpleRNG 39320192934
    assertEqual "SimpleRNG should produce the expected value"
        (-71247759271294, SimpleRNG (-4669293151603464775)) (nextInt rng)

test_numrec = TestCase $ do
    let st0tup = (13918343841046207510,17853252737462582934,15339173004872320485)
        st1tup = (5351857992332895613,4508453503337492835,15541049524600368728)
        rng0 = numRecRNG 29912
        intval = -3624315724536508186 :: Int
    assertEqual "numRecRNG should have expected state given seed"
        (NumRecRNG st0tup) (rng0)
    assertEqual "numRecRNG should generate expected value given seed"
        (intval, NumRecRNG st1tup) (nextInt rng0)
    assertEqual "numRecRNG should generate expected value using generate seed"
        (intval, NumRecRNG st1tup) (generate rng0 :: (Int, NumRecRNG))

test_generate = TestCase $ do
    let rng = SimpleRNG 39320192934
    assertEqual "SimpleRNG should generate expected int"
        (-71247759271294) (fst $ generate rng :: Int)

test_generateR = TestCase $ do
    let rng = SimpleRNG 39320192934
    assertEqual "SimpleRNG should generate expected int"
        (-71247759271294) (fst . generateR randInt $ rng)
    assertEqual "SimpleRNG should generate expected nonNegative int"
        (71247759271293) (fst . generateR nonNegativeInt $ rng)
    assertEqual "SimpleRNG should generate expected nonNegativeEven int"
        (71247759271292) (fst . generateR nonNegativeIntEven $ rng)

random_tests = [TestLabel "test ints" test_ints,
                TestLabel "test SimpleRNG" test_simple,
                TestLabel "test NumRecRNG" test_numrec,
                TestLabel "test generate" test_generate,
                TestLabel "test generateR" test_generateR]
