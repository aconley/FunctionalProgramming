module CandyMachineTests (candy_machine_tests) where

import Test.HUnit
import Control.Applicative
import FPInScala.State.StateMonad
import FPInScala.State.CandyMachine

test_candyseq = TestCase $ do
    let inp = [Coin, Turn, Coin, Turn, Coin, Turn]
        init_state = Machine True 10 5
    in assertEqual "Should have 14, 1 candies, coins"
        (14, 1) (fst $ runState $ init_state)