module FPInScala.Laziness (
    myTake,
    myDrop,
    myTakeWhile,
    forAll,
    takeWhileViaFoldr,
    ones,
    constant,
    from,
    fibs,
    unfold,
    fibsUnfold,
    fromUnfold,
    constantUnfold,
    onesUnfold,
    mapUnfold,
    takeUnfold,
    zipWithUnfold,
    zipAll
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

-- Generate fibonacci sequence
fibs :: [Int]
fibs = fibInner 0 1
    where fibInner a b = a : fibInner b (a + b)

-- corecursive generator: given an initial state (s), produce the
-- next value in a list as well as the new state.  The user
-- provides a function to do so that returns Nothing if the sequence
-- terminates.  We use the built in Maybe rather than our own Option type
unfold :: s -> (s -> Maybe (a, s)) -> [a]
unfold z f = case f z of
    Nothing -> []
    Just (aa, ss) -> aa : unfold ss f

-- fibs using unfold
fibsUnfold :: [Int]
fibsUnfold = unfold (0, 1) (\(a, b) -> Just (a, (b, a + b)))

-- from using unfold
fromUnfold :: Int -> [Int]
fromUnfold x = unfold x (\xx -> Just (xx, xx + 1))

-- const using unfold
constantUnfold :: a -> [a]
constantUnfold v = unfold v (\vv -> Just(vv, vv))

-- ones using unfold
onesUnfold :: [Int]
onesUnfold = constantUnfold 1

-- map via unfold
mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f as = unfold as g
    where g [] = Nothing
          g (a:aa) = Just (f a, aa)

-- take via unfold
takeUnfold :: Int -> [a] -> [a]
takeUnfold n as = unfold (n, as) g
    where g (0, _) = Nothing
          g (m, (a:aa)) = Just (a, (m-1, aa))

-- zipWith via unfold
zipWithUnfold :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithUnfold f as bs = unfold (as, bs) g
    where g ([], _) = Nothing
          g (_, []) = Nothing
          g ((aa:aas), (bb:bbs)) = Just (f aa bb, (aas, bbs))

-- zipAll via unfold
zipAll :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipAll as bs = unfold (as, bs) g
    where g ((aa:aas, bb:bbs)) = Just ((Just aa, Just bb), (aas, bbs))
          g ([], []) = Nothing
          g ([], (bb:bbs)) = Just ((Nothing, Just bb), ([], bbs))
          g ((aa:aas), []) = Just ((Just aa, Nothing), (aas, []))

