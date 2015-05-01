module OptionTests (option_tests) where

import Test.HUnit
import FPInScala.ErrorHandling.Option
import FPInScala.ErrorHandling.OptionExamples

sqfunc :: Int -> Int
sqfunc x = x * x

test_fmap = TestCase $ do
    assertEqual "fmap should return None on None"
        None (fmap sqfunc None)
    assertEqual "fmap should work on Some"
        (Some 25) (fmap sqfunc $ Some 5)

option_tests = [TestLabel "test fmap" test_fmap]
