module Homework3
( Tweet,
  TweetSet,
  singleTweet,
  insert,
  contains,
  union,
  remove,
  filterTweetSet,
  mostRetweeted,
  descendingByRetweet
) where

import Data.List (foldl')

-- This module should be only imported as qualified

data Tweet = Tweet { user :: String, text :: String, 
                     retweets :: Int }

-- Tweets are ordered on the text --
instance Eq Tweet where
    Tweet _ t1 _ == Tweet _ t2 _ = t1 == t2

instance Ord Tweet where
    compare (Tweet _ t1 _) (Tweet _ t2 _) = compare t1 t2

instance Show Tweet where
    show (Tweet u t r) = "{ User: " ++ u ++ " Tweet: " ++ t ++ " Retweets: " ++ show r ++ " }"

-- Another comparison operator based on retweets
compRetweet :: Tweet -> Tweet -> Ordering
(Tweet _ _ r1) `compRetweet` (Tweet _ _ r2) = r1 `compare` r2

-- Greater of two tweets based on retweets
--  returns the first if they are equal
greaterRetweet :: Tweet -> Tweet -> Tweet
greaterRetweet t1 t2 = case t1 `compRetweet` t2 of GT -> t1
                                                   LT -> t2
                                                   EQ -> t1

-- TweetSet is a binary tree of Tweets
data TweetSet = Empty | Node Tweet (TweetSet) (TweetSet) 
                deriving (Show)

-- Make a tree with a single tweet
singleTweet :: Tweet -> TweetSet
singleTweet t = Node t Empty Empty

-- Make a tree from a list of Tweets
listToTweetSet :: [Tweet] -> TweetSet
listToTweetSet ts = foldl' insert Empty ts

-- Insert a tweet into a tree, returning the new tree
--  If a tweet is already found with the same text, replace
--  it with the new one
insert :: TweetSet -> Tweet -> TweetSet
insert Empty newt = singleTweet newt
insert (Node t left right) newt
    | newt < t = Node t (left `insert` newt) right
    | newt > t = Node t left (right `insert` newt)
    | otherwise = Node newt left right 

-- Does the tweet set contain the given Tweet?
contains :: TweetSet -> Tweet -> Bool
contains Empty tweet = False
contains (Node t left right) tweet
    | tweet < t = left `contains` tweet
    | tweet > t = right `contains` tweet
    | otherwise = True -- it's in this node

-- Forms the union of two TweetSets.  This version
-- favors the second set, in that any duplicates
-- come from there
union :: TweetSet -> TweetSet -> TweetSet
union Empty set2 = set2
union set1 Empty = set1
union set1 (Node t left right) = set1 `insert` t `union` left `union` right


-- Remove the specified tweet
remove :: TweetSet -> Tweet -> TweetSet
remove Empty twt = error "Removing from an empty tree"
remove (Node t left right) twt 
    | twt < t = Node t (left `remove` twt) right
    | twt > t = Node t left (right `remove` twt)
    | otherwise = left `union` right -- it matched this node

-- Return a new set of elements that match the predicate
filterTweetSet :: (Tweet -> Bool) -> TweetSet -> TweetSet
filterTweetSet p Empty = Empty
filterTweetSet p (Node t left right) 
    | p(t) = Node t (filterTweetSet p left) (filterTweetSet p right)
    | otherwise = (filterTweetSet p left) `union` (filterTweetSet p right)

-- Return the most retweeted treet in the tree
mostRetweeted :: TweetSet -> Tweet
mostRetweeted Empty = error "Searching empty tree"
mostRetweeted (Node t Empty Empty) = t
mostRetweeted (Node t Empty right) = t `greaterRetweet` (mostRetweeted right)
mostRetweeted (Node t left Empty) = t `greaterRetweet` (mostRetweeted left)
mostRetweeted (Node t left right) =
    let lmaxt = t `greaterRetweet` (mostRetweeted left)
        rmax = mostRetweeted right
    in lmaxt `greaterRetweet` rmax

-- Produce a list of tweets in descending order of retweets
descendingByRetweet :: TweetSet -> [Tweet]
descendingByRetweet Empty = []
descendingByRetweet (Node t Empty Empty) = [t]
descendingByRetweet set =
    let mostret = mostRetweeted set
        rem = set `remove` mostret
    in mostret : (descendingByRetweet rem)