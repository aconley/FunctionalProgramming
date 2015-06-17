module StateMonadTests (state_tests) where

import Test.HUnit hiding (State)
import FPInScala.State.StateMonad

doubler :: State Int Int
doubler = State $ \x -> (x, 2 * x)

tripler :: State Int Int
tripler = State $ \x -> (x, 3 * x)

test_run = TestCase $ do
    assertEqual "doubler should work as expected"
        (4, 8) (runState doubler 4)
    assertEqual "tripler should work as expected"
        (15, 45) (runState tripler 15)

test_fmap = TestCase $ do
    assertEqual "fmap should apply function to return value"
        (12, 18) (runState (fmap (+3) doubler) 9)

test_mapS2 = TestCase $ do
    let s1 = mapS2 (+) doubler tripler
        s2 = mapS2 (+) tripler doubler
    assertEqual "map2 should apply functions in order"
        (30, 60) (runState s1 10)
    assertEqual "map2 should apply functions in order when reversed"
        (40, 60) (runState s2 10)

test_sequenceS = TestCase $ do
    let s = sequenceS [doubler, tripler, tripler]
    assertEqual "sequenceS should chain operations in order"
        ([5, 10, 30], 90) (runState s 5)


state_tests = [TestLabel "test runState" test_run,
               TestLabel "test fmap" test_fmap,
               TestLabel "test mapS2" test_mapS2,
               TestLabel "test sequenceS" test_sequenceS]
