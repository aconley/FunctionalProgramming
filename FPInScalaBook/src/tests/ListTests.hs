module ListTests (list_tests) where

import Test.HUnit
import FPInScala.DataStructures.List
import Data.Foldable (foldMap)

test_mytail = TestCase $ do
    assertEqual "mytail should give empty list on single element list"
        [] (mytail [2])
    assertEqual "mytail should give the tail" [3, 5] (mytail [1, 3, 5])

test_sethead = TestCase $ do
    assertEqual "setHead should make a single element list in" [4] (setHead 4 [])
    assertEqual "setHead should change the head in"
        [4, 3, 4] (setHead 4 [2, 3, 4])

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

test_foldleftsum = TestCase $ do
    assertEqual "foldLeftSum should compute the sum of a single element list"
        (foldLeftSum [1]) 1
    assertEqual "foldLeftSum should compute the sum of a list"
        (foldLeftSum [1, 3, 7, 11, 19]) 41

test_foldleftproduct = TestCase $ do
    let list1 = [1, 3, 7, 11, -3]
    assertEqual "foldLeftProduct should compute the prod of a single element list"
        (foldLeftProduct [1]) 1
    assertEqual "foldLeftProduct should compute the product of a list"
        (foldLeftProduct list1) (product list1)

test_foldleftlength = TestCase $ do
    assertEqual "foldLeftLength should compute the length of an empty list"
        (foldLeftLength []) 0
    assertEqual "foldLeftLength should compute the length of a single element list"
        (foldLeftLength [4]) 1
    assertEqual "foldLeftLength should compute the length of a list"
        (foldLeftLength [1, 3, 7, 11, 19]) 5

test_reversefold = TestCase $ do
    let elist = [] :: [Int]
        test_list1 = [1]
        test_list2 = [5, 7, 9, 11, 22]
        test_list3 = [9, 4, 1, 21, 19, 37]
    assertEqual "reverseFold should reverse an empty list"
        (reverseFold elist) elist
    assertEqual "reverseFold should reverse test list 1"
        (reverseFold test_list1) (reverse test_list1)
    assertEqual "reverseFold should reverse test list 2"
        (reverseFold test_list2) (reverse test_list2)
    assertEqual "reverseFold should reverse test list 1"
        (reverseFold test_list3) (reverse test_list3)

test_append = TestCase $ do
    let list1 = [1, 3, 5, 7]
        list2 = [9, 2, 1]
    assertEqual "append should work with empty lists"
        (append list1 []) list1
    assertEqual "append should work with empty lists"
        (append [] list1) list1
    assertEqual "append should append two lists"
        (append list1 list2) (list1 ++ list2)
    assertEqual "append should append two lists"
        (append list2 list1) (list2 ++ list1)

test_myconcat = TestCase $ do
    let list1 = [[1, 7], [11], [-22, -7], []]
    assertEqual "myconcat should concatenate"
        (myconcat list1) (concat list1)

test_mymap = TestCase $ do
    let list1 = [3, 7, 9, 11, -1, 22]
    assertEqual "mymap should work like map"
        (mymap (+1) list1) (map (+1) list1)

test_mymapright = TestCase $ do
    let list1 = [3, 7, 9, 11, -1, 22]
    assertEqual "mymapRight should work like map"
        (mymapRight (+1) list1) (map (+1) list1)

test_myfilter = TestCase $ do
    let list1 = [2, 6, 11, 14, -2, 3]
    assertEqual "myfilter should return an empty list when given one"
        (myfilter [] (> 5)) []
    assertEqual "myfilter should work like filter"
        (myfilter list1 (> 3)) (filter (> 3) list1)

test_myfilterright = TestCase $ do
    let list1 = [2, 6, 11, 14, -2, 3]
    assertEqual "myfilterRight should return an empty list when given one"
        (myfilterRight [] (< 5)) []
    assertEqual "myfilter should work like filter"
        (myfilterRight list1 (< 3)) (filter (< 3) list1)

test_flatmap = TestCase $ do
    let list1 = [2, 6, 11, 14]
        elist = [] :: [Int]
    assertEqual "flatMap on empty list should be empty"
        (flatMap elist (\x -> [x, x])) elist
    assertEqual "flatMap should work like Data.Foldable.foldMap"
        (flatMap list1 (\x -> [x, x])) (foldMap (\x -> [x, x]) list1)

test_myzipwith = TestCase $ do
    let elist = [] :: [Int]
        list1 = [1, 2, 4, 6]
        list2 = [3, 7, -4]
    assertEqual "myzipWith should return empty list on empty left"
        (myzipWith elist list1 (+)) elist
    assertEqual "myzipWith should return empty list on empty right"
        (myzipWith list1 elist (-)) elist
    assertEqual "myzipWith should return empty list on both empty"
        (myzipWith elist elist (*)) elist
    assertEqual "myzipWith should work like zipWith on general lists"
            (myzipWith list1 list2 (+)) (zipWith (+) list1 list2)

list_tests = [TestLabel "test mytail" test_mytail,
              TestLabel "test setHead" test_sethead,
              TestLabel "test drop" test_drop,
              TestLabel "test dropWhile" test_dropwhile,
              TestLabel "test init" test_init,
              TestLabel "test foldLeftSum" test_foldleftsum,
              TestLabel "test foldLeftProduct" test_foldleftproduct,
              TestLabel "test foldLeftLength" test_foldleftlength,
              TestLabel "test reverseFold" test_reversefold,
              TestLabel "test append" test_append,
              TestLabel "test myconcat" test_myconcat,
              TestLabel "test mymap" test_mymap,
              TestLabel "test mymapRight" test_mymapright,
              TestLabel "test myfilter" test_myfilter,
              TestLabel "test myfilterright" test_myfilterright,
              TestLabel "test flatMap" test_flatmap]
