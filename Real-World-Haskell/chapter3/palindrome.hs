module Main where
  palindrome :: [a] -> [a]
  palindrome [] = []
  palindrome x = x ++ reverse x
