module Anagrams where

{- 
   Anagrams

   Based on Homework 5 of Functional Programming in Scala from Coursera
-}

import Data.List (groupBy, sort)
import Data.Char (toLower)
import Control.Applicative ((<$>))

{-
  An occurence list is a list of char, int pairs, sorted by
  the character.  Each pair represents the number of times
  a given character appears.  Thus, the second element must
  be positive, and the character is kept in lower case.
  Each character should only appear once.
-}
data Occurrence = Occurrence { char :: Char, getCount :: Int } deriving (Eq)
type Occurrences = [Occurrence]

-- Occurrence lists are sorted by the character
instance Ord Occurrence where
    compare (Occurrence a _) (Occurrence b _) = compare a b

instance Show Occurrence where
    show (Occurrence c n) = "(" ++ [c] ++ ", " ++ show n ++ ")"

-- Convert a single word (no spaces) to an occurrence list
makeWordOccurrences :: String -> Occurrences
makeWordOccurrences "" = []
makeWordOccurrences str = 
    let lowstr = map toLower str
    in map (\x -> Occurrence (head x) (length x)) $ groupBy (==) $ sort lowstr

-- Convert a list of words to an occurrence list
makeSentenceOccurrences :: [String] -> Occurrences
makeSentenceOccurrences [] = []
makeSentenceOccurrences strs = makeWordOccurrences $ concat strs

-- Generate all the valid Occurrence combos counting up the
--  number; so ('a', 2) -> [('a', 1), ('a', 2)]
-- Note that the output isn't an occurrence list, but a list of
--  occurences because the same character appears multiple times.
generateSubOccurrence :: Occurrence -> [Occurrence]
generateSubOccurrence (Occurrence a 0) = []
generateSubOccurrence (Occurrence c n) = 
  map (\x -> Occurrence (fst x) (snd x)) $ zip (repeat c) [1..n]

{- 
   Generate all subsets of an Occurrences, where this includes
   sub occurrences; so, for example

    [('a' 2), ('b' 1)] -> [[], [('b' 1)], [('a' 1), ('b' 1)],
                           [('a' 2), ('b' 1)], [('a' 1)], [('a' 2)]

   in some order
-}
combinations :: Occurrences -> [Occurrences]
combinations [] = [[]]
combinations (x:[]) = [] : (map ( :[]) $ generateSubOccurrence x)
combinations (x:xs) = 
    let this_sub = generateSubOccurrence x
        prev_sub = combinations xs
        prev_plus_new = [ e : p | e <- this_sub, p <- prev_sub ]
    in prev_sub ++ prev_plus_new

{-
  This is the subtraction accumulator for sub.  The output is
  accumulated in reverse order for efficiency.
-}
subAcc :: Occurrences -> Occurrences -> Maybe Occurrences -> Maybe Occurrences
subAcc _ _ Nothing = Nothing
subAcc [] [] acc = acc  -- All done, lists consumed
subAcc [] subtrahend _ = Nothing -- subtrahend was not subset of minuend
subAcc minuend [] (Just acc) =  -- Done processing subtrahend
    let revminu = reverse minuend
    in Just (revminu ++ acc)
subAcc (m:ms) subt@(s:ss) (Just acc) =   --Most interesting case
  case compare m s of GT -> Nothing   -- subtrahend not subset of minuend
                      LT -> subAcc ms subt $ Just $ m : acc
                      EQ -> let c = char m
                                n1 = getCount m
                                n2 = getCount s
                            in case compare n1 n2 of
                              GT -> let newOcc = Occurrence c (n1 - n2)
                                    in subAcc ms ss $ Just $ newOcc : acc
                              LT -> Nothing  -- again, not a subset
                              EQ -> subAcc ms ss (Just acc)

{-
  Subtraction of two occurrence lists.  Uses Maybe to handle
  failure, which occurs when the second object is not a strict
  subset of the first.

  As an example:
     [('a', 2), ('b', 1)] - [('a', 1), ('b', 1)] = Just [('a', 1)]
-}
sub :: Occurrences -> Occurrences -> Maybe Occurrences
minuend `sub` subtrahend = 
    reverse <$> subAcc minuend subtrahend (Just []) 
