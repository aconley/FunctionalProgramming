module Main where
  isPalindrome :: (Eq a) => [a] -> Bool
  isPalindrome [] = True
  isPalindrome x = x == reverse x
