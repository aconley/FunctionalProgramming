module Main where
  import Data.List (sortBy)

  sortlen :: [[a]] -> [[a]]
  sortlen xs = sortBy (\x y -> compare (length x) (length y)) xs 
