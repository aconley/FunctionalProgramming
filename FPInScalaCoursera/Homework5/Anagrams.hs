module Anagrams (buildDictionary,
                 Dictionary,
                 getAnagrams) where

{- 
   Anagrams

   Based on Homework 5 of Functional Programming in Scala from Coursera

   This is the user facing bit
-}

import qualified Data.Map as Map
import Anagrams_Internal

-- Build a dictionary from a list of words; assumes all spaces
--  removed
buildDictionary :: [String] -> Dictionary
buildDictionary [] = Map.empty
buildDictionary strs = buildDictionaryAccum strs Map.empty

{-
  Given a dictionary and a sentence, returns a list of anagrams,
  returning Nothing if it can't match
-}
getAnagrams :: Dictionary -> String -> Maybe [String]
getAnagrams dict "" = Nothing
getAnagrams dict sentence | Map.null dict = Nothing
getAnagrams dict sentence = do
  occ <- makeSentenceOccurrences $ words sentence
  anagramList <- getAnagramsAccum dict [(occ, [])] []
  restitchOutput anagramList                 
    
