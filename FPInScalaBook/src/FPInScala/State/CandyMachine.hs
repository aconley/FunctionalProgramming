module FPInScala.State.CandyMachine (
    Input(..),
    simulateMachine,
    Machine(..)
    ) where

import FPInScala.State.StateMonad

data Input = Coin | Turn

-- Machine state; locked, candies, coins
data Machine = Machine Bool Int Int
    deriving (Eq, Show)

-- Each action returns the number of coins and candies in the machine as its value
-- State transition on coin insert
insertCoin :: Machine -> ((Int, Int), Machine)
--  no candies -- return coin, don't modify state
insertCoin (Machine l 0 coins) = ((0, coins), Machine l 0 coins)
-- Locked machine -- unlock
insertCoin (Machine True candies coins) = 
    ((candies, coins+1), Machine False candies (coins+1))
-- Ignore everything else
insertCoin (Machine l candies coins) = ((candies, coins), Machine l candies coins)

-- And knob turn
turnKnob :: Machine -> ((Int, Int), Machine)
--  no candies -- return coin, don't modify state
turnKnob (Machine l 0 coins) = ((0, coins), Machine l 0 coins)
-- Unlocked machine -- distribute candy
turnKnob (Machine False candies coins) = 
    ((candies - 1, coins), Machine True (candies - 1) coins)
-- Ignore everything else
turnKnob (Machine l candies coins) = ((candies, coins), Machine l candies coins)

-- Maps from a list of Inputs to a list of State actions
inputMap :: [Input] -> [State Machine (Int, Int)]
inputMap xs = map f xs
    where f Coin = State insertCoin
          f Turn = State turnKnob

-- Runs the simulation
simulateMachine :: [Input] -> State Machine (Int, Int)
simulateMachine inp = fmap last $ sequenceS (inputMap inp)
