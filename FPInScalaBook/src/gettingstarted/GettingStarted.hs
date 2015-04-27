factorial :: Int -> Int
factorial n = go n 1
   where go n acc | n <= 0 = acc
                  | otherwise = go (n - 1) (n * acc)
