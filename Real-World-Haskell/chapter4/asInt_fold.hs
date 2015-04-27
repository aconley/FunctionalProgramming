import Data.List (foldl')
import Data.Char (digitToInt)

asInt_fold :: String -> Int
asInt_fold xall@(x:xs) = if x == '-' then -asInt' xs else asInt' xall
    where asInt' y = foldl' (\a b -> a*10 + digitToInt b) 0 y
