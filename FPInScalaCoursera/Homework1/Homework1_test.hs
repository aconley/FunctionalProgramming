module Homework1_test where

-- Unit tests --
import Test.HUnit
import Homework1

test_pascal = TestCase $ do
  assertEqual "Pascal(0,2) should be 1" 1 (pascal 0 2)
  assertEqual "Pascal(1,3) should be 3" 3 (pascal 1 3)
  assertEqual "Pascal(1,2) should be 2" 2 (pascal 1 2)
  assertEqual "Pascal(2,4) should be 6" 6 (pascal 2 4)
  assertEqual "Pascal(1,5) should be 5" 5 (pascal 1 5)

test_balance = TestCase $ do
  assertEqual ":-) is not balanced" False (balanceParen ":-)")
  assertEqual "(if (zero? x) max(/ 1x)) is balanced" 
    True (balanceParen "(if (zero? x) max(/1 x))")
  assertEqual "())( is not balanced" False (balanceParen "())(")

test_change = TestCase $ do
  assertEqual "There are 3 ways to make change for 4 with 1 2"
   3 (countChange 4 [1, 2])
  assertEqual "There are 4 ways to make change for 4 with 1 2 3"
   4 (countChange 4 [1, 2, 3])
  assertEqual "There are 2 ways to make change for 5 with 1 3"
   2 (countChange 5 [1, 3])

tests = TestList [TestLabel "test pascal" test_pascal,
                  TestLabel "test balance" test_balance,
                  TestLabel "test change" test_change]

main = runTestTT tests