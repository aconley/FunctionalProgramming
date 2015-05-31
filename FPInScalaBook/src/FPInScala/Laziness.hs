module FPInScala.Laziness (
    myTake,
    myDrop,
    myTakeWhile,
    forAll,
    takeWhileViaFoldr,
    ones,
    constant,
    from
    ) where

-- This chapter is a touch silly in Haskell, since
-- it is lazy by default.

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x:xs)
    | n <= 0 = []
    | otherwise = x : myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop _ []= []
myDrop n xs@(_:ys)
    | n <= 0 = xs
    | otherwise = myDrop (n - 1) ys

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
    | p x = x : myTakeWhile p xs
    | otherwise = []

-- Test if a predicate holds for all elements
forAll :: (a -> Bool) -> [a] -> Bool
forAll _ [] = True
forAll p (x:xs)
    | p x = forAll p xs
    | otherwise = False

-- takeWhile done with foldr
takeWhileViaFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileViaFoldr p xs = foldr (\y ys -> if p y then y:ys else []) [] xs

-- Infinite series of 1s
ones :: [Int]
ones = 1 : ones

-- Infinite series of user specified value
constant :: a -> [a]
constant v = v : constant v

-- Ascending sequence starting at user value
from :: Int -> [Int]
from x = x : from (x + 1)

