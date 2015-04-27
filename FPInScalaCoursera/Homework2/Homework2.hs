module Homework2 (Set, 
                  emptySet, singletonSet, listtoSet,
                  unionSet, intersectSet, diffSet, filterSet,
                  forall, exists, mapSet) where

-- Basic set type, defined as a function which
-- checks if an element is in the set
type Set a = a -> Bool

-- Part 1 -- basic functions on sets

-- First, some constructors -- more than the exercise specifies!

-- Empty set
emptySet :: Set a
emptySet = \_ -> False

-- Constructs a set consisting of a single element
singletonSet :: (Eq a) => a -> Set a
singletonSet val = \x -> x == val

-- Constructs a set from a list
listtoSet :: (Eq a) => [a] -> Set a
listtoSet val = \x -> x `elem` val

-- some operations on sets

-- Union of two sets
unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet s t = \x -> s x || t x -- can be in either

-- Intersection of two sets
intersectSet :: (Eq a) => Set a -> Set a -> Set a
intersectSet s t = \x -> s x && t x -- must be in both

-- Difference of two sets -- elements in set one but not two
diffSet :: (Eq a) => Set a -> Set a -> Set a
diffSet s t = \x -> s x && not (t x)

-- Apply an arbitrary predicate to a Set
-- Predicate has the same type as Set, but it isn't one
--  so don't label it thusly.  This is the same as intersect, really
filterSet :: (Eq a) => Set a -> (a -> Bool) -> Set a
filterSet s p = \x -> s x && p x

-- Section 2 -- Queries and transformations on sets
-- To keep it slightly simpler, limit set elements to 0 to 100
--  rather than -1000 to 1000 and specify to ints

-- Determines whether the predicate is true for all set elements
forall :: Set Int -> (Int -> Bool) -> Bool
forall s p = iter [0..100]
    where iter [] = True
          iter (x:xs)
            | (s x) == False = iter xs
            | p x = iter xs
            | otherwise = False

-- Determine whether predicate is true for any value in set
--  We are supposed to use forall, but that's horribly inefficient - oh well
exists :: Set Int -> (Int -> Bool) -> Bool
exists s p = not $ forall s (not . p)

-- Map operation on set.  Returns a new set with the
--  function applied
mapSet :: Set Int -> (Int -> Int) -> Set Int
mapSet s f = listtoSet . map f $ filter s [0..100]

                          