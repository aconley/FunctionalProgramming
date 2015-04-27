module Main (main) where

import Test.HUnit
import GSTests (gettingStarted_tests)
import ListTests (list_tests)

main = do
   runTestTT gettingStarted_tests
   runTestTT list_tests
