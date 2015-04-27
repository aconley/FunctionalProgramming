module Huffman (HuffmanTree, 
                createHuffmanTree, 
                decode, 
                encode,
                createHuffmanMap,
                quickEncode) where

{- Huffman coding 
   Based on Homework4 of Functional Programming in Scala
    from Coursera -- but not quite following their specifications
   This was pretty easy until I decided to start playing games
    with Maybe values for error control, which descended into fun
    with the Maybe Monad and applicative functors
-}

import Data.List (groupBy, sort, sortBy, insert)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Map as Map

data HuffmanTree = Leaf { char :: Char, weight :: Int } 
             | Fork { left :: HuffmanTree, right :: HuffmanTree, 
                      chars :: String, weight :: Int } deriving(Eq)

instance Show HuffmanTree where
    show (Leaf c w) = "Leaf("++ [c] ++ ", " ++ show w ++ ")"
    show (Fork l r c w) = "Fork(" ++ show l ++ " , " ++ show r ++ ", " ++ c ++ ", " ++ show w ++ ")"

-- Order by weight
instance Ord HuffmanTree where
    compare l r = compare (weight l) (weight r)

-- Need to have one function for both Leaf and Fork
getChars :: HuffmanTree -> String
getChars (Leaf c _) = [c]
getChars (Fork _ _ cs _) = cs

-- Form a codeTree by combining two smaller ones, joining the chars and
--  weights
makeHuffmanTree :: HuffmanTree -> HuffmanTree -> HuffmanTree
makeHuffmanTree l r = Fork l r (getChars l ++ getChars r) (weight l + weight r)
                                      
-- Frequency count of letters
--  Returns a tuple of character, number of occurrences
times :: String -> [(Char, Int)]
times str = map (\x -> (head x, length x)) $ groupBy (==) $ sort str

-- Makes list of Leafs from a string ordered (ascending) by frequency
makeOrderedLeafList :: String -> [HuffmanTree]
makeOrderedLeafList str = 
    let ordlist = sortBy (\x y -> compare (snd x) (snd y)) $ times str
    in map (\x -> Leaf (fst x) (snd x)) ordlist

-- Given an ordered list of HuffmanTrees, combine the first two elements
--  (which have the lowest weight by the ordering rule) into a new
--  Fork, then insert it into the remainder in the position to retain
--  the ordering
combine :: [HuffmanTree] -> [HuffmanTree]
combine [] = error "Calling combine on empty list"
combine (x:[]) = [x]
combine (x:y:[]) = [makeHuffmanTree x y]
combine (x:y:xs) = insert (makeHuffmanTree x y) xs

-- Create a Huffman code Tree from an input string
createHuffmanTree :: String -> HuffmanTree
createHuffmanTree [] = error "Empty string"
createHuffmanTree (x:[]) = Leaf x 1
createHuffmanTree str = 
    let lflist = makeOrderedLeafList str
        red = until (\x -> length x == 1) combine lflist
    in head red

-- Encoding and decoding operations
type HuffmanCode = [Int]
type HuffmanMap = Map.Map Char HuffmanCode

-- Decode a single character from a Huffman Code, returning
--  the character and the unused bits.  Use Maybe for error
--  handling
decodeSingle :: HuffmanCode -> HuffmanTree -> Maybe (Char, HuffmanCode)
decodeSingle xs (Leaf c _) = Just (c, xs)
decodeSingle [] (Fork _ _ _ _) = Nothing
decodeSingle (x:xs) (Fork l r cs _)
    | x == 0 = decodeSingle xs l
    | x == 1 = decodeSingle xs r
    | otherwise = Nothing

-- Decode a Huffman encoded string into an accumulator
--- Accumulate in reversed order because it's faster to prepend new elements
decodeAcc :: HuffmanCode -> HuffmanTree -> Maybe String -> Maybe String
decodeAcc _ _ Nothing = Nothing
decodeAcc [] _ acc = acc
decodeAcc xs h acc =
    case decodeSingle xs h of
      Nothing -> Nothing
      Just (newchar, remain) -> decodeAcc remain h (fmap (newchar:) acc)

-- Decode a Huffman encoded string using Maybe to handle failure
decode :: HuffmanTree -> HuffmanCode -> Maybe String
decode _ [] = Nothing
decode h xs = reverse <$> (decodeAcc xs h (Just []))

-- Encode a single character using a Huffman tree
--  into an accumulator.  The accumulator is in reversed
--  order because it is more efficient to append
encodeSingleAcc :: Char -> HuffmanTree -> HuffmanCode -> HuffmanCode
encodeSingleAcc c (Leaf c2 _) acc
    | c == c2 = acc -- Found it
    | otherwise = error $ "Failed to encode " ++ show c
encodeSingleAcc c h acc 
    | c `elem` (getChars $ left h) = encodeSingleAcc c (left h) (0:acc)
    | otherwise = encodeSingleAcc c (right h) (1:acc)

-- Encode a single character using a Huffman tree
--  not very efficient!  The returned Huffman code is
--  reversed for efficiency (later fixed in encode)
encodeSingle :: Char -> HuffmanTree -> Maybe HuffmanCode
encodeSingle c h
  | c `elem` (getChars h) = Just (encodeSingleAcc c h [])
  | otherwise = Nothing

-- unsafe encoding used when you are sure a character is in a tree
--  note that -- as usual -- the encoding is reversed
encodeSingleUnsafe :: Char -> HuffmanTree -> HuffmanCode
encodeSingleUnsafe c h = encodeSingleAcc c h []

-- Encode reversed code into accumulator for given string
--  Again, we accumulate on the front under the assumption
--  that the code for one character will be much shorter
--  than the stuff we have already built up, so it will be
--  faster than appending to the end
encodeAcc :: String -> HuffmanTree -> Maybe HuffmanCode -> Maybe HuffmanCode
encodeAcc _ _ Nothing = Nothing
encodeAcc [] _ acc = acc
encodeAcc (x:[]) h acc = (++) <$> (encodeSingle x h) <*> acc
encodeAcc (x:xs) h acc = 
    let newcode = encodeSingle x h
    in encodeAcc xs h ((++) <$> newcode <*> acc)

-- Encode a string using a Huffman tree
--  This both reverses the final computation, and handles failure
--  via the Maybe monad
encode :: HuffmanTree -> String -> Maybe HuffmanCode
encode _ [] = Nothing
encode h (x:[]) = fmap reverse (encodeSingle x h)
encode h xs = encodeAcc xs h (Just []) >>= revHuff
    where revHuff hc = Just (reverse hc)

-- Use a map to make this faster to apply; this provides
--  a map from characters to their huffman codes
createHuffmanMap :: HuffmanTree -> HuffmanMap
createHuffmanMap h = Map.fromList p
    where p = [(c, reverse $ encodeSingleUnsafe c h) | c <- chars h]

-- Quick encoding of a string
--  codemap is [Maybe HuffmanCode].  sequence turns this into
--   Maybe [HuffmanCode], respecting failure, then we concat using <$> (fmap)
quickEncode :: HuffmanMap -> String -> Maybe HuffmanCode
quickEncode h [] = Nothing
quickEncode hm str = 
    let codemap = map (\c -> Map.lookup c hm) str
    in concat <$> sequence codemap

