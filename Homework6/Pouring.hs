module Pouring (solvePouring) where

{-
  Water pouring problem

  We don't do a lot of bounds checking because this is just toy code
-}

import Data.List (intersperse)
import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

-- Both of these are assumed to start at index 0
-- They are functionally identical, but given different
--  types to avoid confusion
data Capacity = Capacity { getCapacity :: Seq.Seq Int } deriving (Eq)
data State = State { getState :: Seq.Seq Int } deriving (Eq)

instance (Show) Capacity where
    show (Capacity a) = 
        let n = Seq.length a
        in show n ++ " bottles with capacity " ++ (show $ Fold.toList a)

instance (Ord) Capacity where
    compare (Capacity a) (Capacity b) = compare a b

instance (Show) State where
    show (State a) = 
        let n = Seq.length a
        in show n ++ " bottles with state " ++ (show $ Fold.toList a)

instance (Ord) State where
    compare (State a) (State b) = compare a b

listToCapacity :: [Int] -> Capacity
listToCapacity cs = Capacity $ Seq.fromList cs

-- Get the initial state of all 0s
initialState :: Capacity -> State
initialState (Capacity a) = State $ Seq.replicate (Seq.length a) 0

-- See if a specified final state is present in a state
stateElem :: State -> Int -> Bool
stateElem (State ss) elem = Fold.elem elem ss

-- Move type: three types of moves
--   Empty a glass
--   Fill a glass
--   Pour from one to another
data Move = Empty Int | Fill Int | Pour Int Int deriving (Eq)

instance (Show) Move where
    show (Empty a) = "Empty glass " ++ show a
    show (Fill a) = "Fill glass " ++ show a
    show (Pour from to) = "Pour from " ++ show from ++ " to " ++ show to

-- Define ordering so that we can use some of the objects below in a set
instance (Ord) Move where
    compare (Empty a) (Empty b) = compare a b
    compare (Empty a) _ = LT
    compare (Fill a) (Empty b) = GT
    compare (Fill a) (Fill b) = compare a b
    compare (Fill a) _ = LT
    compare (Pour a b) (Pour c d) = 
        case a `compare` c of
          EQ -> b `compare` c
          GT -> GT
          LT -> LT
    compare (Pour a b) _ = GT

-- this applies a move to a state
-- Make the capacity the first argument so this can be curried
applyMove :: Capacity -> State -> Move -> State
applyMove _ (State ss) (Empty i) = 
    State $ Seq.update i 0 ss 
applyMove (Capacity cs) (State ss) (Fill i) =
    let cap = cs `Seq.index` i
    in State $ Seq.update i cap ss
applyMove (Capacity cs) (State ss) (Pour from to) =
    let state_from = ss `Seq.index` from
        cap_to = cs `Seq.index` to
        state_to = ss `Seq.index` to
        amount = min state_from (cap_to - state_to)
        update_from = Seq.update from (state_from - amount) ss
    in State $ Seq.update to (state_to + amount) update_from

-- A move list is just a list of moves with the
-- most recent move at the head
type MoveList = [Move]

showMoves :: MoveList -> String
showMoves [] = ""
showMoves mvs = concat $ intersperse ", " $ reverse $ map show mvs

-- Given a capacity and an initial position, give the
--  final state for a MoveList.
-- Recall that a MoveList is in reverse order, so use a foldr
getEndState :: Capacity -> State -> MoveList -> State
getEndState _ init [] = init
getEndState cap init mvs = foldr (\mv st -> applyMove cap st mv) init mvs

-- A path is a list of moves from an initial state
--  The most recent move is at the start
-- For convenience, we keep the capacity and final state as well
data Path = Path { capacity :: Capacity, initState ::  State,
                   moves :: MoveList, finalState :: State } deriving (Eq)

instance (Show) Path where
    show path@(Path cap init [] _) = "Initial state: " ++ show init
    show path@(Path _ _ moves final) =
        showMoves moves ++ " --> " ++ show final

-- We base Eq, Ord on the final state, then moves
instance (Ord) Path where
    compare (Path c1 i1 m1 f1) (Path c2 i2 m2 f2) = 
        case f1 `compare` f2 of
          GT -> GT
          LT -> LT
          EQ -> case m1 `compare` m2 of
                  GT -> GT
                  LT -> LT
                  EQ -> i1 `compare` i2

-- Generate an initial path
initialPath :: Capacity -> State -> Path
initialPath cap init = Path cap init [] init

-- Add a new move to a path
addMove :: Path -> Move -> Path
addMove path@(Path cap _ moves final) mv =
    let newEndState = applyMove cap final mv
        newMoves = mv : moves
    in path { moves=newMoves, finalState=newEndState }

-- Apply a MoveList to a path
applyMoves :: Path -> MoveList -> Path
applyMoves initPath [] = initPath
applyMoves path@(Path cap _ mvs final) moves = 
    let newEndState = getEndState cap final moves
        newMoveList = moves ++ mvs  -- append at front
    in path { moves=newMoveList, finalState=newEndState }

-- Generate all possible new moves from a given state
-- Note what we give back is not a MoveList since it doesn't
--  actually correspond to a coherent set.
generateNewMoves :: State -> [Move]
generateNewMoves (State st) =
    let nm1 = Seq.length st - 1
        empty_moves = [Empty j | j <- [0..nm1]]
        fill_moves = [Fill j | j <- [0..nm1]]
        pour_moves = [Pour i j | i <- [0..nm1], j <- [0..nm1], i /= j]
    in empty_moves ++ fill_moves ++ pour_moves


type PathSet = Set.Set Path
type StateSet = Set.Set State

-- Generate candidate new paths from current ones that don't
-- end up in already explored state
generateNewPaths :: Path -> StateSet -> PathSet
generateNewPaths path@(Path cap init moves final) explored=
    let newmoves = generateNewMoves final
        newset = Set.fromList $ map (\mv -> addMove path mv) newmoves
    in Set.filter (\p -> (finalState p) `Set.notMember` explored) newset

-- From a set of paths, lazily generate all new paths
generateAllNewPaths :: PathSet -> StateSet -> [PathSet]
generateAllNewPaths ps explored =
    let nextSets = Set.map (\p -> generateNewPaths p explored) ps
        union = Set.foldl Set.union Set.empty nextSets
        newExplored = (Set.map finalState union) `Set.union` explored
    in union : generateAllNewPaths union newExplored

-- check if a target state is in the final state of a path
pathElem :: Path -> Int -> Bool
pathElem path elem = stateElem (finalState path) elem

-- Extract the actual paths that have the target state
pathSetGetElem :: PathSet -> Int -> PathSet
pathSetGetElem pset elem = Set.filter (\p -> pathElem p elem) pset

-- check if a target state is in any final state of a PathSet
pathSetElem :: PathSet -> Int -> Bool
pathSetElem pset elem = not $ Set.null $ pathSetGetElem pset elem

-- This solves the pouring problem starting from an initial empty state
findPaths :: Capacity -> Int -> Maybe PathSet
findPaths cap@(Capacity c) e
          | Seq.null c = Nothing
          | e < 0 = Nothing
          | e == 0 && Fold.maximum c >= 0 = 
              Just $ Set.singleton $ initialPath cap $ initialState cap 
          | e > (Fold.maximum c) = Nothing
findPaths cap e =
    let init = initialState cap
        initPathSet = Set.singleton $ initialPath cap init
        initExplored = Set.singleton init
        possiblePaths = generateAllNewPaths initPathSet initExplored
        solvedPathSets = filter (\ps -> pathSetElem ps e) possiblePaths
        solutionList = map (\ps -> pathSetGetElem ps e) solvedPathSets
    in Just $ head solutionList

-- Convert solutions into a string
stringifySolutions :: PathSet -> [String]
stringifySolutions paths | Set.null paths = []
stringifySolutions paths = 
    Fold.toList $ Set.map show paths

-- Solve the pouring problem for initial empty state
solvePouring :: [Int] -> Int -> String
solvePouring [] _ = "No solution found"
solvePouring c e =
    let cap = listToCapacity c
        solutions = findPaths cap e
    in case solutions of
         Nothing -> "No solutions found"
         Just sol -> 
             let solutionString = stringifySolutions sol
             in concat $ intersperse " -OR- " solutionString
    
    