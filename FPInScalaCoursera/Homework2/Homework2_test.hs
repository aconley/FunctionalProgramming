module Homework2_test where

-- Unit tests --
import Test.HUnit
import Homework2

teste = TestCase $ assertEqual 
  "Set should be empty" False (emptySet 3)
tests1 = TestCase $ assertEqual "Should find element in singleton" 
  True (singletonSet 3 3)
tests2 = TestCase $ assertEqual "Should not find element in singleton" 
  False (singletonSet 3 5)
testl1 = TestCase $ assertEqual "Should find element in list"
  True (listtoSet [3, 4, 5] 5)
testl2 = TestCase $ assertEqual "Should not find element in list"
  False (listtoSet [3, 4, 5] 1)

testunion = TestCase $ do
  let joinl = (listtoSet [1, 2, 3]) `unionSet` (listtoSet [6, 7, 8])
      joins = (singletonSet 3) `unionSet` (singletonSet 5)
  assertEqual "Should find in union" True (joinl 8)
  assertEqual "Should not find in untion" False (joinl 4)
  assertEqual "Should find in union" True (joins 5)
  assertEqual "Should not find in union" False (joins 4)

testdiff= TestCase $ do
  let joinl = (listtoSet [1, 2, 3]) `diffSet` (listtoSet [2, 7])
  assertEqual "Should find in diff" True (joinl 1)
  assertEqual "Should not find in diff" False (joinl 2)
  assertEqual "Should find in diff" True (joinl 3)
  assertEqual "Should not find in diff" False (joinl 7)

testintersect = TestCase $ do
  let isectl = (listtoSet [1, 3, 5]) `intersectSet` (listtoSet [1, 4])
  assertEqual "Should find in intersection" True (isectl 1)
  assertEqual "Should not find in intersection" False (isectl 3)
  assertEqual "Should not find in intersection" False (isectl 4)
  assertEqual "Should not find in intersection" False (isectl 5)

testfilt = TestCase $ do
  let s1 = listtoSet [1, 3, 5]
      s2 = singletonSet 4
      f = \x -> x > 3
      fs1 = filterSet f s1
      fs2 = filterSet f s2
  assertEqual "Should find 5 in filtered set 1" True (fs1 5)
  assertEqual "Should not find 1 in filtered set 1" False (fs1 1)
  assertEqual "Should not find 3 in filtered set 1" False (fs1 3)
  assertEqual "Should not find 11 in filtered set 1" False (fs1 11)
  assertEqual "Should find 4 in filtered set 2" True (fs2 4)
  assertEqual "Should not find 3 in filtered set 2" False (fs2 3)

testforall = TestCase $ do
  let s1 = listtoSet [3, 5, 7]
      s2 = (singletonSet 1) `unionSet` s1
      f = \x -> x >= 3
  assertEqual "Should be true for set 1" True (forall s1 f)
  assertEqual "Should not be true for set 2" False (forall s2 f)

testexists = TestCase $ do
  let s1 = listtoSet [10, 15, 25]
      f1 = \x -> x > 20
      f2 = \x -> x < 0
  assertEqual "Should be an element > 20" True (exists s1 f1)
  assertEqual "Should not be any < 0" False (exists s1 f2)

testmap = TestCase $ do
  let s1 = listtoSet [11, 14, 8]
      f = \x -> x + 2
      ms = mapSet s1 f
  assertEqual "13 should be present now" True (ms 13)
  assertEqual "11 should not be present now" False (ms 11)
  assertEqual "14 should not be present now" False (ms 14)
  assertEqual "16 should be present now" True (ms 16)
  assertEqual "8 should not be present now" False (ms 8)
  assertEqual "10 should be present now" True (ms 10)

tests = TestList [TestLabel "test_empty" teste,
                  TestLabel "test_singleton1" tests1,
                  TestLabel "test_singleton2" tests2,
                  TestLabel "test_list1" testl1,
                  TestLabel "test_list2" testl2,
                  TestLabel "test_union" testunion,
                  TestLabel "test_intersection" testintersect,
                  TestLabel "test_diff" testdiff,
                  TestLabel "test_filter" testfilt,
                  TestLabel "test_forall" testforall,
                  TestLabel "test_exists" testexists,
                  TestLabel "test_map" testmap]

main = runTestTT tests