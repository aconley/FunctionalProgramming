module Anagrams where

{- 
   Anagrams

   Based on Homework 5 of Functional Programming in Scala from Coursera
-}

import Data.List (groupBy, sort)
import Data.Char (toLower)

{-
  An occurence list is a list of char, int pairs, sorted by
  the character.  Each pair represents the number of times
  a given character appears.  Thus, the second element must
  be positive, and the character is kept in lower case.
  Each character should only appear once.
-}
data Occurrence = Occurrence Char Int deriving (Eq)
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