-- Examples of using the Option data type

module FPInScala.ErrorHandling.OptionExamples (
    optMean,
    optVariance,
    optAbs) where

import FPInScala.ErrorHandling.Option

optMean :: (Floating a) => [a] -> Option a
optMean [] = None
optMean xs = Some $ sum xs / (fromIntegral $ length xs)

varX :: (Floating a) => [a] -> a -> Option a
varX xs mn = optMean (map (\x -> (x - mn)**2) xs)

optVariance :: (Floating a) => [a] -> Option a
optVariance xs = do
    mn <- optMean xs
    var <- varX xs mn
    return var

optAbs :: (Num a) => Option a -> Option a
optAbs = lift abs
