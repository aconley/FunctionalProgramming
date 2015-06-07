module StateTests (state_tests) where

import Test.HUnit
import FPInScala.State
import Data.Word (Word64)

test_ints = TestCase $ do
    assertEqual "ints should return an empty list when n = 0"
        [] (fst $ ints (SimpleRNG 0x9384ABF20) 0)
    assertEqual "ints should return an empty list when n < 0"
        [] (fst $ ints (SimpleRNG 0x9384ABF20) (-3))
    assertEqual "ints should return an 5 element when n = 5"
        (5) (length . fst $ ints (SimpleRNG 0x9384ABF20) 5)

state_tests = [TestLabel "test ints" test_ints]
