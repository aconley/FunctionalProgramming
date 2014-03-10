module Pouring where

{-
  Water pouring problem

  We don't do a lot of bounds checking because this is just toy code
-}

import Data.List (intersperse)
import Data.Array (Array(..), elems, (!), listArray, (//))

-- Both of these are assumed to start at index 0
-- They are functionally identical, but given different
--  types to avoid confusion
data Capacity = Capacity { getCapacity :: Array Int Int }
data State = State { getState :: Array Int Int }

listToCapacity :: [Int] -> Capacity
listToCapacity cs = 
    let n = length cs
        arr = listArray (0, n - 1) cs
    in Capacity arr

getNCapacity :: Capacity -> Int
getNCapacity c = length $ elems $ getCapacity c

getNState :: State -> Int
getNState s = length $ elems $ getState s

instance (Show) Capacity where
    show cap@(Capacity a) = 
        let n = getNCapacity cap
            arrcont = show $ elems a
        in show n ++ " bottles with capacity " ++ arrcont

instance (Show) State where
    show s@(State a) = 
        let n = getNState s
            arrcont = show $ elems a
        in show n ++ " bottles with state " ++ arrcont

-- Get the initial state of all 0s
initialState :: Capacity -> State
initialState cap = 
    let n = getNCapacity cap
    in State $ listArray (0, n - 1) $ repeat 0

-- Move type: three types of moves
--   Empty a glass
--   Fill a glass
--   Pour from one to another
data Move = Empty Int | Fill Int | Pour Int Int

instance (Show) Move where
    show (Empty a) = "Empty glass " ++ show a
    show (Fill a) = "Fill glass " ++ show a
    show (Pour from to) = "Pour from " ++ show from ++ " to " ++ show to

-- this applies a move to a state
-- Make the capacity the first argument so this can be curried
applyMove :: Capacity -> State -> Move -> State
applyMove _ (State ss) (Empty i) = 
    let newarr = ss // [(i, 0)]
    in State newarr
applyMove (Capacity cs) (State ss) (Fill i) =
    let cap = cs ! i
        newarr = ss // [(i, cap)]
    in State newarr
applyMove (Capacity cs) (State ss) (Pour from to) =
    let state_from = ss ! from
        cap_to = cs ! to
        state_to = ss ! to
        amount = min state_from (cap_to - state_to)
        newfrom = (from, state_from - amount)
        newto = (to, state_to + amount)
    in State (ss // [newfrom, newto])

-- A move list is just a list of moves with the
-- most recent move at the end.
type MoveList = [Move]

showMoves :: MoveList -> String
showMoves [] = ""
showMoves mvs = concat $ intersperse ", " $ reverse $ map show mvs

-- Given a capacity and an initial position, give the
-- final state for a MoveList
getEndState :: Capacity -> State -> MoveList -> State
getEndState _ init [] = init
getEndState cap init mvs = foldr (\mv st -> applyMove cap st mv) init mvs

-- A path is a list of moves from an initial state
--  The most recent move is at the start
-- For convenience, we keep the capacity and final state as well
data Path = Path { capacity :: Capacity, initState ::  State,
                   moves :: MoveList, finalState :: State }

instance (Show) Path where
    show path@(Path cap init moves final) =
        showMoves moves ++ " --> " ++ show final

-- Add a new move to a path
addMove :: Path -> Move -> Path
addMove path@(Path cap _ moves final) mv =
    let newEndState = applyMove cap final mv
        newMoves = mv : moves
    in path { moves=newMoves, finalState=newEndState }

-- Generate all possible new moves from a given state
-- Note what we give back is not a MoveList since it doesn't
--  actually correspond to a coherent set.
generateNewMoves :: State -> [Move]
generateNewMoves st =
    let nm1 = getNState st - 1
        empty_moves = [Empty j | j <- [0..nm1]]
        fill_moves = [Fill j | j <- [0..nm1]]
        pour_moves = [Pour i j | i <- [0..nm1], j <- [0..nm1], i /= j]
    in empty_moves ++ fill_moves ++ pour_moves