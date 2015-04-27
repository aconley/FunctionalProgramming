module FPInScala.DataStructures.List (
    mytail,
    setHead,
    mydrop,
    mydropWhile,
    myinit)
where

mytail :: [a] -> [a]
mytail [] = error "Tail of empty list"
mytail (x:xs) = xs

setHead :: a -> [a] -> [a]
setHead y [] = [y]
setHead y (x: xs) = y : xs

mydrop :: [a] -> Int -> [a]
mydrop xs n | n < 0 = error "n must be positive in mydrop"
mydrop xs 0 = xs
mydrop (x:xs) n = mydrop xs (n-1)

mydropWhile :: [a] -> (a -> Bool) -> [a]
mydropWhile [] _ = []
mydropWhile xs@(x:xs') p
    | p x = mydropWhile xs' p
    | otherwise = xs

myinit :: [a] -> [a]
myinit [x] = []
myinit (x:xs) = x : init xs
myinit [] = error "Can't take init of empty list"
