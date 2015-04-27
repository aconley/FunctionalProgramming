module ListTests (list_tests) where

import Test.HUnit
import FPInScala.DataStructures.List

test_mytail = TestCase $ do
    assertEqual "mytail should give empty list on single element list"
        (mytail [2]) []
    assertEqual "mytail should give the tail" (mytail [1, 3, 5]) [3, 5]

test_sethead = TestCase $ do
    assertEqual "setHead should make a single element list in" (setHead 4 []) [4]
    assertEqual "setHead should change the head in"
        (setHead 4 [2, 3, 4]) [4, 3, 4]

test_drop = TestCase $ do
    assertEqual "dropping 0 elements should give same list"
        (mydrop [1, 2] 0) [1, 2]
    assertEqual "dropping should drop the right elements"
        (mydrop [1, 2, 3, 4] 2) [3, 4]

test_dropwhile = TestCase $ do
    assertEqual "dropWhile on true should empty list"
        (mydropWhile [1, 2, 3] (\x -> True)) []
    assertEqual "dropWhile should drop specified elements"
        (mydropWhile [2, 3, 4, 5, 6] (\x -> x < 4)) [4, 5, 6]

test_init = TestCase $ do
    assertEqual "init of single element list should be empty"
        (myinit [1]) []
    assertEqual "init of list should return all but last element"
        (myinit [1, 2, 3, 4, 5]) [1, 2, 3, 4]

list_tests = TestList [TestLabel "test mytail" test_mytail,
                       TestLabel "test setHead" test_sethead,
                       TestLabel "test drop" test_drop,
                       TestLabel "test dropWhile" test_dropwhile,
                       TestLabel "test init" test_init]
