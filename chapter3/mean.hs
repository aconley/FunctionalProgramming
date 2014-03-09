module Main where
  mean :: (Floating a) => [a] -> Maybe a
  mean [] = Nothing
  mean x = Just ( (sum x) / (fromIntegral $ length x) )
