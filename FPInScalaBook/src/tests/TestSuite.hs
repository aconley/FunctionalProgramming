module Main (main) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import GSTests (gettingStarted_tests)
import ListTests (list_tests)
import TreeTests (tree_tests)
import OptionTests (option_tests)

all_tests = gettingStarted_tests ++ list_tests ++ tree_tests ++
    option_tests
tests = hUnitTestToTests $ TestList all_tests

main = defaultMain tests
