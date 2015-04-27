module Main where

  safeHead :: [a] -> Maybe a
  safeHead [] = Nothing
  safeHead (x:xs) = Just x

  safeTail :: [a] -> Maybe [a]
  safeTail [] = Nothing
  safeTail (x:[]) = Just [x]
  safeTail (x:xs) = Just xs

  safeLast :: [a] -> Maybe a
  safeLast [] = Nothing
  safeLast (x:[]) = Just x
  safeLast (x:xs) = safeLast xs

  safeInit :: [a] -> Maybe [a]
  safeInit [] = Nothing
  safeInit x = Just (init x)
