module Main where
  import Data.List (foldl')
  import Data.Char (digitToInt, isDigit)

  sfunc :: Char -> Int
  sfunc x 
     | isDigit x == True = digitToInt x
     | otherwise = error $ "Not a digit: '" ++ [x] ++ "'"

  asInt :: String -> Int
  asInt [] = error "No input"
  asInt ('-':xs) = - asInt xs
  asInt ('+':xs) = asInt xs
  asInt (x:xs) = foldl' (\acc x -> 10 * acc + sfunc x) 0 (x:xs)
