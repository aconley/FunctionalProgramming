module FPInScala.GettingStarted (
    factorial,
    fibonacci,
    isSorted) where

-- Compute n!
factorial :: Int -> Int
factorial n = go n 1
   where go n acc | n <= 0 = acc
                  | otherwise = go (n - 1) (n * acc)

-- Get the nth Fibonacci number
fibonacci :: Int -> Int
fibonacci n = fibInner n 0 1
    where fibInner 0 prev _ = prev
          fibInner idx prev curr  = fibInner (idx-1) curr (prev + curr)

-- Test if an array is sorted using the user provided ordered test
isSorted :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
isSorted f [] = True
isSorted f (x:[]) = True
isSorted f (x:rest@(y:ys)) | f x y == False = False
                           | otherwise = isSorted f rest


