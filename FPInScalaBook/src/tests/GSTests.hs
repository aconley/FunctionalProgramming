module GSTests (gettingStarted_tests) where

import Test.HUnit
import FPInScala.GettingStarted

test_factorial = TestCase $ do
    assertEqual "factorial 5 should be 120" (factorial 5) 120
    assertEqual "factorial 1 should be 1" (factorial 1) 1
    assertEqual "factorial 0 should be 1" (factorial 0) 1
    assertEqual "factorial 10 should be 3628800" (factorial 10) 3628800

test_fibonnaci = TestCase $ do
    assertEqual "Fibonacci 1 should be 1" (fibonacci 1) 1
    assertEqual "Fibonacci 10 should be 55" (fibonacci 10) 55
    assertEqual "Fibonacci 12 should be 144" (fibonacci 12) 144

test_isSorted = TestCase $ do
    assertBool "Single element list should be sorted" (isSorted (<) [1])
    assertBool "Test list should be sorted with <" (isSorted (<) [1, 2, 3, 11])
    assertBool "Test list should not be sorted with <" (not $ isSorted (<) [3, 2, 1])
    assertBool "Test list should be sorted with >" (isSorted (>) [3, 2, 1])
    assertBool "Test list should not be sorted with <" (not $ isSorted (<) [1, 1])
    assertBool "Test list should be sorted with <=" (isSorted (<=) [1, 1, 1])

gettingStarted_tests = [TestLabel "test factorial" test_factorial,
                        TestLabel "test fibonacci" test_fibonnaci,
                        TestLabel "test isSorted" test_isSorted]
