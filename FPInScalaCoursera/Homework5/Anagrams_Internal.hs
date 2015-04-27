module Anagrams_Internal where

{- 
   Internal parts of Anagrams

   Separated so that testing code can access what it needs
-}

import Data.List (groupBy, sort, nub)
import Data.Char (toLower, isAlpha)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Control.Applicative ((<$>))
import qualified Data.Map as Map

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
--  But, since we are going to use these inside a Map, we
--  have to provide a full comparison in case a, b are the same
instance Ord Occurrence where
    compare (Occurrence ca va) (Occurrence cb vb) 
            | ca == cb = compare va vb
            | otherwise = compare ca cb

instance Show Occurrence where
    show (Occurrence c n) = "(" ++ [c] ++ ", " ++ show n ++ ")"

-- Convert a single word (no spaces) to an occurrence list
makeWordOccurrences :: String -> Maybe Occurrences
makeWordOccurrences "" = Nothing
makeWordOccurrences str = 
    let lowstr = map toLower str
        grouped = groupBy (==) $ sort lowstr
    in Just $ map (\x -> Occurrence (head x) (length x)) grouped 

-- Convert a list of words to an occurrence list
makeSentenceOccurrences :: [String] -> Maybe Occurrences
makeSentenceOccurrences [] = Nothing
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

    [('a' 2), ('b' 1)] -> Just [[('b' 1)], [('a' 1), ('b' 1)],
                           [('a' 2), ('b' 1)], [('a' 1)], [('a' 2)]

   in some order.  Note that, unlike what the homework calls for,
   we don't return the empty list, but using Maybe to handle empty lists.
-}
combinations :: Occurrences -> Maybe [Occurrences]
combinations [] = Nothing
combinations (x:[]) = Just (map ( :[]) $ generateSubOccurrence x)
combinations (x:xs) = 
    let this_sub = generateSubOccurrence x
        prev_sub = combinations xs
    in case prev_sub of
         Nothing -> Nothing
         Just prev -> let newbit = [ e : p | e <- this_sub, p <- prev ]
                          singleton = map (:[]) this_sub
                      in Just (singleton ++ prev ++ newbit)

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
subAcc (m@(Occurrence c1 n1):ms) subt@((Occurrence c2 n2):ss) (Just acc)
  | c1 > c2 = Nothing     -- subtrahend not subset of minuend
  | c1 < c2 = subAcc ms subt $ Just $ m : acc
  | (c1 == c2) && (n1 == n2) = subAcc ms ss (Just acc) -- perfect removal
  | (c1 == c2) && (n1 < n2) = Nothing  -- again, not a subset
  | otherwise = let newOcc = Occurrence c1 (n1 - n2)
                in subAcc ms ss $ Just $ newOcc : acc

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


{-
  Dictionary type -- a map from an occurrence list to a list of words
   that have that occurrence list
-}
type Dictionary = Map.Map Occurrences [String]

{-
  Accumulator for building the dictionary
-}
buildDictionaryAccum :: [String] -> Dictionary -> Dictionary
buildDictionaryAccum [] dict = dict
buildDictionaryAccum (s:ss) dict =
    let occ = makeWordOccurrences s
    in case occ of 
         Nothing -> buildDictionaryAccum ss dict
         Just occl -> let key = [map toLower s]
                          newdict = Map.insertWith (++) occl key dict
                      in buildDictionaryAccum ss newdict

-- Get all anagrams of a given word from a dictionary
getWordAnagrams :: Dictionary -> String -> Maybe [String]
getWordAnagrams dict _ | Map.null dict = Nothing
getWordAnagrams dict "" = Nothing
getWordAnagrams dict str =
    let occ = makeWordOccurrences str
    in case occ of
         Nothing -> Nothing
         Just occl -> Map.lookup occl dict

{-
  Given a dictionary, and occurence list, and a word,
  checks if the word is in the dictionary and can be subtracted
  from the previous occurence list.  If so, returns 
  Just (newOcc, anagrams), where newOcc is the new occurrence list
  and anagrams are the list of anagrams of the word.
  In all other cases returns Nothing
-}
tryWord :: Dictionary -> Occurrences -> String -> Maybe (Occurrences, [String])
tryWord dict [] _ = Nothing
tryWord dict occs wrd = do
  occur <- makeWordOccurrences wrd
  anagrams <- Map.lookup occur dict
  newocc <- occs `sub` occur
  return (newocc, anagrams)


foundInMapAccum :: (Ord k) => Map.Map k v -> [k] -> [(k, v)] -> [(k, v)]
foundInMapAccum m [] accum = accum
foundInMapAccum m (k:ks) accum = 
    let srch = Map.lookup k m
    in case srch of 
         Nothing -> foundInMapAccum m ks accum
         Just val -> foundInMapAccum m ks ((k, val) : accum)

{-
  Given a Map and a list of potential keys, return only those
  elements from keys that are found in the map along with their
  associated values
-}
foundInMap :: (Ord k) => Map.Map k v -> [k] -> Maybe [(k, v)]
foundInMap m ks | Map.null m = Nothing   -- short circuit on empty map
foundInMap m ks = 
    case foundInMapAccum m ks [] of
      [] -> Nothing
      prs -> Just prs


-- The current search state is a combination of the remaining occurrence
--  list and the current anagram list
type SearchState = (Occurrences, [String])

{-
  Make new search state from a single new Occurrence, Word list
-}
makeNewSearchState :: SearchState -> (Occurrences, [String]) 
                   -> Maybe [SearchState]
makeNewSearchState ([], _) _ = Nothing
makeNewSearchState (occ, words) (newocc, newwords) = 
    let newOccList = fromJust $ occ `sub` newocc
        newAnagrams = map (:words) newwords
    in Just $ zip (repeat newOccList) newAnagrams

{-
  Updates search state using list of occurrence, word pairs,
  generating a list of new search states from the possibilities
-}
generateNewSearchStates :: SearchState -> [(Occurrences, [String])] 
                  -> Maybe [SearchState]
generateNewSearchStates ([], _) _ = Nothing
generateNewSearchStates state (p:[]) = makeNewSearchState state p
generateNewSearchStates state poss = 
    case mapMaybe (\p -> makeNewSearchState state p) poss of
      [] -> Nothing
      ss -> Just $ concat ss

iterSearchState :: Dictionary -> SearchState -> Maybe [SearchState]
iterSearchState dict state@(occ, words) = do
  comb <- combinations occ
  poss <- foundInMap dict comb
  generateNewSearchStates state poss

getAnagramsAccum :: Dictionary -> [SearchState] -> [[String]] 
                 -> Maybe [[String]]
getAnagramsAccum dict sentence accum | Map.null dict = Nothing
getAnagramsAccum _ [] accum = Just accum
getAnagramsAccum dict state accum =
    let newstates = concat $ mapMaybe (\s -> iterSearchState dict s) state
        (done, continue) = span (null . fst) newstates
        newaccum = (map snd done) ++ accum
    in getAnagramsAccum dict continue newaccum
  
restitchOutput :: [[String]] -> Maybe [String]
restitchOutput [] = Nothing
restitchOutput ss = Just $ map unwords ss
