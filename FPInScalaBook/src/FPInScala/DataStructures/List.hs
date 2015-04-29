module FPInScala.DataStructures.List (
    mytail,
    setHead,
    mydrop,
    mydropWhile,
    myinit,
    mylength,
    foldLeftSum,
    foldLeftProduct,
    foldLeftLength,
    reverseFold,
    append,
    myconcat,
    mymap,
    mymapRight,
    myfilter,
    myfilterRight,
    flatMap,
    myzipWith)
where

mytail :: [a] -> [a]
mytail [] = error "Tail of empty list"
mytail (_:xs) = xs

setHead :: a -> [a] -> [a]
setHead y [] = [y]
setHead y (_: xs) = y : xs

mydrop :: [a] -> Int -> [a]
mydrop _ n | n < 0 = error "n must be positive in mydrop"
mydrop xs 0 = xs
mydrop (_:xs) n = mydrop xs (n-1)

mydropWhile :: [a] -> (a -> Bool) -> [a]
mydropWhile [] _ = []
mydropWhile xs@(x:xs') p
    | p x = mydropWhile xs' p
    | otherwise = xs

myinit :: [a] -> [a]
myinit [_] = []
myinit (x:xs) = x : init xs
myinit [] = error "Can't take init of empty list"

mylength :: [a] -> Int
mylength xs = foldr (\_ y -> y + 1) 0 xs

foldLeft :: [a] -> b -> (b -> a -> b) -> b
foldLeft [] z _ = z
foldLeft (x:xs) z f = foldLeft xs (f z x) f

foldLeftSum :: (Num a) => [a] -> a
foldLeftSum xs = foldLeft xs 0 (+)

foldLeftProduct :: (Num a) => [a] -> a
foldLeftProduct xs = foldLeft xs 1 (*)

foldLeftLength :: [a] -> Int
foldLeftLength xs = foldLeft xs 0 (\x _ -> x + 1)

reverseFold :: [a] -> [a]
reverseFold xs = foldl (flip (:)) [] xs

-- Append one list to another
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- fold version of concat
myconcat :: [[a]] -> [a]
myconcat l = foldr (++) [] l

-- Recursive version of map
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = (f x) : (mymap f xs)

-- Foldr version of map
mymapRight :: (a -> b) -> [a] -> [b]
mymapRight f as = foldr (\x bs -> (f x) : bs) [] as

-- Reimplementation of filter
myfilter :: [a] -> (a -> Bool) -> [a]
myfilter [] _  = []
myfilter (x:xs) f = if f x then x : myfilter xs f
    else myfilter xs f

-- Another reimplementation, this time with foldr
myfilterRight :: [a] -> (a -> Bool) -> [a]
myfilterRight xs f = foldr (\x y -> if f x then x : y else y) [] xs

flatMap :: [a] -> (a -> [b]) -> [b]
flatMap xs f = concat $ map f xs

-- Reimplementation of zipWith
myzipWith :: [a] -> [b] -> (a -> b -> c) -> [c]
myzipWith [] _ _ = []
myzipWith _ [] _ = []
myzipWith (x:xs) (y:ys) f = (f x y) : myzipWith xs ys f
