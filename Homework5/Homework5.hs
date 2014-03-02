module Main (main) where

import Test.HUnit
import Anagrams

test_ord = TestCase $ do
  assertEqual "(c, 1) should be  < (d, 3)" 
    (compare (Occurrence 'c' 1) (Occurrence 'd' 3)) (LT)
  assertEqual "(c, 1) should be  > (a, 3)" 
    (compare (Occurrence 'c' 1) (Occurrence 'a' 3)) (GT)


test_makewo = TestCase $ do
  let test_str = "aardvark"
      exp_list = [Occurrence 'a' 3, Occurrence 'd' 1, Occurrence 'k' 1, 
                  Occurrence 'r' 2, Occurrence 'v' 1]
  assertEqual "aardvark should have expected occurrence list"
    exp_list (makeWordOccurrences test_str)
  assertBool "makeWordOccurrences on empty list should be empty"
    (null $ makeWordOccurrences "")

test_makeso = TestCase $ do
  let test_str = "An aardvark"
      exp_list = [Occurrence 'a' 4, Occurrence 'd' 1, Occurrence 'k' 1, 
                  Occurrence 'n' 1, Occurrence 'r' 2, Occurrence 'v' 1]
  assertBool "makeSentenceOccurrences on an empty list should be empty"
    (null $ makeSentenceOccurrences [])
  assertEqual "'An aardvark' should have expected occurrence list"
    exp_list (makeSentenceOccurrences $ words test_str)

test_gensub = TestCase $ do
  let test_occ = Occurrence 'x' 3
      exp_list = [Occurrence 'x' 1, Occurrence 'x' 2, Occurrence 'x' 3]
  assertEqual "Should generate correct sub occurrences"
    exp_list (generateSubOccurrence test_occ)

test_comb = TestCase $ do
  let test_occ = Occurrence 'x' 4
      test_occlist = [Occurrence 'a' 2, Occurrence 'b' 1]
      expl = [[], [Occurrence 'b' 1], [Occurrence 'a' 1],
              [Occurrence 'a' 1, Occurrence 'b' 1], [Occurrence 'a' 2],
              [Occurrence 'a' 2, Occurrence 'b' 1]]
  assertEqual "combinations of empty list should be empty"
     (combinations []) ([[]])
  assertEqual "combinations of single element should be like gensub"
     (combinations [test_occ]) 
     ([] : (map (:[]) $ generateSubOccurrence test_occ))
  assertEqual "combinations of [('a', 2), ('b', 1)] should be as expected"
     (combinations test_occlist) (expl)

tests = TestList [TestLabel "test Ordering" test_ord,
                  TestLabel "test makeWordOccurrences" test_makewo,
                  TestLabel "test makeSentenceOccurrences" test_makeso,
                  TestLabel "test generateSubOccurrence" test_gensub,
                  TestLabel "test combinations" test_comb]

main = runTestTT tests