module CandyMachineTests (candy_machine_tests) where

import Test.HUnit
import Control.Applicative
import FPInScala.State.StateMonad
import FPInScala.State.CandyMachine

test_simulatemachine = TestCase $ do
    let inp = [Coin, Turn, Coin, Turn, Coin, Turn]
        init_state = Machine True 10 5
        machine = simulateMachine inp
    assertEqual "Should have 14, 1 candies, coins"
        (14, 1) (fst $ runState machine init_state)

candy_machine_tests = [TestLabel "test simulateMachine" test_simulatemachine]
