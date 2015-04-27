module Bloxorz where

{-
  Bloxorz solver
-}

-- Position is specified by row/column counting
--  from upper left.
data Pos = Pos Int Int deriving(Eq, Show)

instance (Ord) Pos where
    compare (Pos r1 c1) (Pos r2 c2) =
        case compare r1 r2 of
          EQ -> compare c1 c2
          GT -> GT
          LT -> LT

-- Change position row
changePosRow :: Pos -> Int -> Pos
changePosRow (Pos r c) dr = Pos (r + dr) c

-- Change position column
changePosCol :: Pos -> Int -> Pos
changePosCol (Pos r c) dc = Pos r (c + dc)

-- Test row equality
rowPosEq :: Pos -> Pos -> Bool
rowPosEq (Pos r1 _) (Pos r2 _) = r1 == r2

-- Test col equality
colPosEq :: Pos -> Pos -> Bool
colPosEq (Pos _ c1) (Pos _ c2) = c1 == c2

-- A block is specified by the positions of its two
--  constituents.  Block 1 has a lower or equal row and
--  column than block 2.  This needs to instance Ord because
--  we will eventually want a set of all positions visited
data Block = Block Pos Pos deriving(Eq, Show)

-- Ord will be from the position of Block 1 first, then Block 2
instance (Ord) Block where
    compare (Block p1 p2) (Block p3 p4) = 
        case compare p1 p3 of
          EQ -> compare p2 p4
          LT -> LT
          GT -> GT

-- Is a block standing up?
isStanding (Block p1 p2) = p1 == p2

-- Are the two rows of a block equal?
rowBlockEq (Block p1 p2) = rowPosEq p1 p2

-- Are the two cols of a block equal?
colBlockEq (Block p1 p2) = colPosEq p1 p2

-- Routines to move blocks around.  First some
--  simple internal convenience functions

-- Moving a block by row
changeBlockRow :: Block -> Int -> Int -> Block
changeBlockRow (Block p1 p2) dr1 dr2 = 
    Block (changePosRow p1 dr1) (changePosRow p2 dr2)

-- Moving a block by col
changeBlockCol :: Block -> Int -> Int -> Block
changeBlockCol (Block p1 p2) dc1 dc2 = 
    Block (changePosCol p1 dc1) (changePosCol p2 dc2)

-- Move data type.  Left, Right already taken by Either type
-- so we append Move to the name
data Move = LeftMove | RightMove | UpMove | DownMove deriving(Eq, Show)

-- Internal helper move functions
--  Right is + col, Down is + row
moveLeft :: Block -> Block
moveLeft block 
    | isStanding block = changeBlockCol block (-2) (-1)  -- Flop over
    | rowBlockEq block = changeBlockCol block (-1) (-2)  -- Stand up
    | otherwise = changeBlockCol block (-1) (-1)  -- Roll right

moveRight :: Block -> Block
moveRight block 
    | isStanding block = changeBlockCol block 1 2
    | rowBlockEq block = changeBlockCol block 2 1
    | otherwise = changeBlockCol block 1 1

moveUp :: Block -> Block
moveUp block
    | isStanding block = changeBlockRow block (-2) (-1)
    | colBlockEq block = changeBlockRow block (-1) (-2)
    | otherwise = changeBlockRow block (-1) (-1)

moveDown :: Block -> Block
moveDown block
    | isStanding block = changeBlockRow block 1 2
    | colBlockEq block = changeBlockRow block 2 1
    | otherwise = changeBlockRow block 1 1

-- Function that applies moves
applyMove :: Block -> Move -> Block
applyMove block LeftMove = moveLeft block
applyMove block RightMove = moveRight block
applyMove block UpMove = moveUp block
applyMove block DownMove = moveDown block

-- Function that produces all possible future moves from given position
--  as a list of the new positions
generateNeighbors :: Block -> [(Block, Move)]
generateNeighbors block =
    let moves = [LeftMove, RightMove, UpMove, DownMove]
        newBlocks = map (applyMove block) moves
    in zip newBlocks moves

-- Terrain is a function from a position to booleans
--  that returns true when the position is valid
type Terrain = Pos -> Bool

-- This checks whether a block is legal
isLegal :: Terrain -> Block -> Bool
isLegal t (Block p1 p2) = (t p1) && (t p2)

-- Simple test terrain type
infiniteTerrain :: Terrain
infiniteTerrain = \p -> True

-- Generates only legal blocks
generateLegalNeighbors :: Terrain -> Block -> [(Block, Move)]
generateLegalNeighbors t b = 
    let n = generateNeighbors b
        pred = (isLegal t) . fst
    in filter pred n
