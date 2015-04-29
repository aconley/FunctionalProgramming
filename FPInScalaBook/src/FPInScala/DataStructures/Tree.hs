module FPInScala.DataStructures.Tree (
    Tree(..),
    size,
    treeMax,
    depth,
    treeMap,
    treeFold,
    sizeFold,
    depthFold,
    treeMapFold
    ) where

-- Basic tree type
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

-- Compute number of nodes and branches in a Tree
size :: Tree a -> Int
size (Leaf _) = 1
size (Branch l r) = 1 + size l + size r

-- Get the maximum value in the tree
treeMax :: (Ord a) => Tree a -> a
treeMax (Leaf x) = x
treeMax (Branch l r) = max (treeMax l) (treeMax r)

-- Maximum path length in tree
depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch l r) = 1 + max (depth l) (depth r)

-- map over tree
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf $ f x
treeMap f (Branch l r) = Branch (treeMap f l) (treeMap f r)

-- tree fold; we need a leaf function and a branch join function
treeFold :: (a -> b) -> (b -> b -> b) -> Tree a -> b
treeFold f _ (Leaf x) = f x
treeFold f g (Branch l r) = g (treeFold f g l) (treeFold f g r)

-- size as tree fold
sizeFold :: Tree a -> Int
sizeFold t = treeFold (\_ -> 1) (\x y -> 1 + x + y) t

-- depth as tree fold
depthFold :: Tree a -> Int
depthFold t = treeFold (\_ -> 1) (\x y -> max x y) t

-- map as tree fold
treeMapFold :: (a -> b) -> Tree a -> Tree b
treeMapFold f t = treeFold (Leaf . f) (Branch) t
