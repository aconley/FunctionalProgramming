module CandyMachineTests (candy_machine_tests) where

import Test.HUnit
import Control.Applicative
import FPInScala.State.StateMonad
import FPInScala.State.CandyMachine

test_simulatemachine = TestCase $ do
    let inp = [Coin, Turn, Coin, Turn, Coin, Turn]
        init_state = Machine True 5 10 -- 5 candies, 10 coins
        machine = simulateMachine inp
    assertEqual "Should have 2 candies, 13 coins"
        (2, 13) (fst $ runState machine init_state)

candy_machine_tests = [TestLabel "test simulateMachine" test_simulatemachine]
