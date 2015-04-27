module Main (main) where

import Test.HUnit
import Anagrams
import Anagrams_Internal
import qualified Data.Map as Map

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
    (Just exp_list) (makeWordOccurrences test_str)
  assertEqual "makeWordOccurrences on empty list should be empty"
    (makeWordOccurrences "") (Nothing)

test_makeso = TestCase $ do
  let test_str = "An aardvark"
      exp_list = Just([Occurrence 'a' 4, Occurrence 'd' 1, Occurrence 'k' 1, 
                       Occurrence 'n' 1, Occurrence 'r' 2, Occurrence 'v' 1])
  assertEqual "makeSentenceOccurrences on an empty list should be empty"
    (makeSentenceOccurrences []) (Nothing)
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
      expl = [[Occurrence 'a' 1], [Occurrence 'a' 2], [Occurrence 'b' 1],
              [Occurrence 'a' 1, Occurrence 'b' 1], 
              [Occurrence 'a' 2, Occurrence 'b' 1]]
  assertEqual "combinations of empty list should be empty"
     (combinations []) (Nothing)
  assertEqual "combinations of single element should be like gensub"
     (Just (map (:[]) $ generateSubOccurrence test_occ))
     (combinations [test_occ]) 
  assertEqual "combinations of [('a', 2), ('b', 1)] should be as expected"
     (Just (expl)) (combinations test_occlist)

test_sub = TestCase $ do
  let x = [Occurrence 'a' 1, Occurrence 'd' 2, Occurrence 'l' 1,
           Occurrence 'r' 1]
      y1 = [Occurrence 'r' 1]
      y2 = [Occurrence 'a' 1, Occurrence 'l' 1]
      y3 = [Occurrence 'd' 1, Occurrence 'l' 1, Occurrence 'r' 1]
      y4 = [Occurrence 'a' 1, Occurrence 'd' 2]
      y5 = [Occurrence 'a' 1, Occurrence 'c' 4, Occurrence 'l' 1]
      x2 = [Occurrence 'd' 1, Occurrence 'i' 1, Occurrence 'l' 1,
            Occurrence 'o' 1, Occurrence 'r' 1, Occurrence 's' 2,
            Occurrence 't' 1, Occurrence 'w' 1]
      y6 = [Occurrence 'i' 1, Occurrence 'l' 1, Occurrence 's' 1,
            Occurrence 't' 1]
      x2my6 = [Occurrence 'd' 1, Occurrence 'o' 1, Occurrence 'r' 1, 
               Occurrence 's' 1, Occurrence 'w' 1]
  assertEqual "Subtracting empty list should do nothing"
    (Just x) (x `sub` []) 
  assertEqual "One element subtraction should remove just that"
    (Just [Occurrence 'a' 1, Occurrence 'd' 2, Occurrence 'l' 1]) (x `sub` y1)
  assertEqual "Two element subtraction should remove those two"
    (Just [Occurrence 'd' 2, Occurrence 'r' 1]) (x `sub` y2) 
  assertEqual "Three element partial subtraction should remove and reduce"
    (Just [Occurrence 'a' 1, Occurrence 'd' 1]) (x `sub` y3)
  assertEqual "Should subtract first to elements"
    (Just [Occurrence 'l' 1, Occurrence 'r' 1]) (x `sub` y4)
  assertEqual "Subtraction of element not in list should result in Nothing"
    (Nothing) (x `sub` [Occurrence 'c' 4])
  assertEqual "Subtraction buried element not in list should be Nothing"
    (Nothing) (x `sub` y5)
  assertEqual "Subtraction of element with larger count should be Nothing"
    (Nothing) (x `sub` [Occurrence 'd' 3])
  assertEqual "Self subtraction should give empty list"
    (Just []) (x `sub` x)
  assertEqual "x2 minus y6 should be as expected"
    (Just x2my6) (x2 `sub` y6)

test_builddict = TestCase $ do
  let word_list = ["This", "list", "words", "hits"]
      dict = buildDictionary word_list
  assertBool "Empty map from empty word list"
     (Map.null $ buildDictionary [])
  assertEqual "fudge should not be in dictionary"
     (Nothing) (getWordAnagrams dict "fudge")
  assertEqual "Should find one word for list occurrence list"
     (Just ["list"]) (getWordAnagrams dict "list")
  assertEqual "Should find one word for slit occurrence list"
     (Just ["list"]) (getWordAnagrams dict "slit")
  assertEqual "Should find two words for shit occurrence list"
     (Just ["hits", "this"]) (getWordAnagrams dict "shit")

tests = TestList [TestLabel "test Ordering" test_ord,
                  TestLabel "test makeWordOccurrences" test_makewo,
                  TestLabel "test makeSentenceOccurrences" test_makeso,
                  TestLabel "test generateSubOccurrence" test_gensub,
                  TestLabel "test combinations" test_comb,
                  TestLabel "test subtraction" test_sub,
                  TestLabel "test building dictionary" test_builddict]

main = runTestTT tests