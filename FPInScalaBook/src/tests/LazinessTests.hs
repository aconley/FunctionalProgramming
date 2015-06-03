module LazinessTests (laziness_tests) where

import Test.HUnit
import FPInScala.Laziness

test_mytake = TestCase $ do
    assertEqual "myTake should return an empty list when given one"
        [] (myTake 4 ([] :: [Int]))
    assertEqual "myTake should return an empty list when taking 0"
        [] (myTake 0 ['a', 'b'])
    assertEqual "myTake should return an empty list when taking negative n"
        [] (myTake (-3) ['a', 'b'])
    assertEqual "myTake should take the expected number of elements"
        [0..3] (myTake 4 [0..10])

test_mydrop = TestCase $ do
    assertEqual "myDrop should return an empty list when given one"
        [] (myDrop 4 ([] :: [Int]))
    assertEqual "myDrop should return the same list when dropping 0"
        ['a', 'b'] (myDrop 0 ['a', 'b'])
    assertEqual "myDrop should return the same list when dropping negative n"
        ['a', 'b'] (myDrop (-3) ['a', 'b'])
    assertEqual "myDrop should drop the expected number"
        [4..10] (myDrop 4 [0..10])

test_infinite = TestCase $ do
    assertEqual "ones should consist of 1s"
        ([1, 1, 1, 1]) (myTake 4 ones)
    assertEqual "constant should consist of expected constant"
        ([3, 3]) (myTake 2 $ constant 3)
    assertEqual "from should produce the expected ascending sequence"
        (take 20 [3..]) (myTake 20 $ from 3)

test_mytakewhile = TestCase $ do
    assertEqual "myTakeWhile should return an empty list when given one"
        [] (myTakeWhile (> 3) ([] :: [Int]))
    assertEqual "myTakeWhile should return an empty list when the first one fails"
        [] (myTakeWhile (> 3) [0..10])
    assertEqual "myTakeWhile should return the expected elements"
        [0..3] (myTakeWhile (< 4) [0..10])

test_forall = TestCase $ do
    assertBool "forAll should return true on an empty list"
         (forAll (\_ -> False) [])
    assertBool "forAll should detect elements that fail"
        (not $ forAll (> 3) [9, 7, 3])
    assertBool "forAll should be true when all elements pass"
        (forAll (> 2) [3..20])

test_takewhilefoldr = TestCase $ do
    assertEqual "takeWhileViaFoldr should return an empty list when given one"
        [] (takeWhileViaFoldr (> 3) ([] :: [Int]))
    assertEqual "takeWhileViaFoldr should return an empty list when the first one fails"
        [] (takeWhileViaFoldr (> 3) [0..10])
    assertEqual "takeWhileViaFoldr should return the expected elements"
        [0..3] (takeWhileViaFoldr (< 4) [0..10])

test_fibs = TestCase $ do
    assertEqual "First 13 elements of fibs should be as expected"
        [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
        (myTake 13 fibs)

test_unfold = TestCase $ do
    assertEqual "First 13 elements of fibsUnfold should be as expected"
        [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
        (myTake 13 fibsUnfold)
    assertEqual "onesUnfold should consist of 1s"
        ([1, 1, 1, 1]) (myTake 4 onesUnfold)
    assertEqual "constantUnfold should consist of expected constant"
        ([3, 3]) (myTake 2 $ constantUnfold 3)
    assertEqual "fromUnfold should produce the expected ascending sequence"
        (take 20 [3..]) (myTake 20 $ fromUnfold 3)
    assertEqual "mapUnfold should work like map"
        (map (*2) [1, 2, 3, 4]) (mapUnfold (*2) [1, 2, 3, 4])
    assertEqual "takeUnfold should work like take"
        (take 10 [4..]) (takeUnfold 10 [4..])
    assertEqual "zipWithUnfold should work like zipWith"
        (zipWith (+) [1..7] [9..11]) (zipWithUnfold (+) [1..7] [9..11])
    assertEqual "zipAll should zip as expected"
        ([(Just 2, Just 3), (Just 4, Nothing)])
        (zipAll [2, 4] [3])
    assertEqual "zipAll should zip as expected"
        ([(Just 2, Just 3), (Nothing, Just 11)])
        (zipAll [2] [3, 11])

test_startswith = TestCase $ do
    assertBool "[1, 3, 7, 9] does start with [1, 3]"
        (startsWith [1, 3, 7, 9] [1, 3])
    assertBool "[1, 3, 7, 9] does not start with [3, 7]"
        (not $ startsWith [1, 3, 7, 9] [3, 7])

test_hassubsequence = TestCase $ do
    assertBool "[1, 3, 7, 9] does contain [3, 7]"
        (hasSubsequence [1, 3, 7, 9] [3, 7])
    assertBool "[1, 3, 7, 9] does not contain [10, 3, 1]"
        (not $ startsWith [1, 3, 7, 9] [10, 3, 1])
    assertBool "[1, 3, 7, 9] does not contain [3, 9] in order"
        (not $ hasSubsequence [1, 3, 7, 9] [3, 9])


laziness_tests = [TestLabel "test myTake" test_mytake,
                  TestLabel "test myDrop" test_mydrop,
                  TestLabel "test myTakeWhile" test_mytakewhile,
                  TestLabel "test forAll" test_forall,
                  TestLabel "test takeWhileViaFoldr" test_takewhilefoldr,
                  TestLabel "test infinite sequences" test_infinite,
                  TestLabel "test Fibonacci generator" test_fibs,
                  TestLabel "test unfold" test_unfold,
                  TestLabel "test startsWith" test_startswith,
                  TestLabel "test hasSubsequence" test_hassubsequence]
