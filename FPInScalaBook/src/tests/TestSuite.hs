module Main (main) where

import Test.HUnit
import GSTests (gettingStarted_tests)

main = do
   runTestTT gettingStarted_tests
