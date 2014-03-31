module Main (main) where

import Test.HUnit
import Bloxorz

testPosEq = TestCase $ do
    assertBool "(1, 1) should be equal to itself"
      ((Pos 1 1) == (Pos 1 1))
    assertBool "(1, 1) should not be equal to (2, 3)"
      ((Pos 1 1) /= (Pos 2 3))

testPosOrd = TestCase $ do
    assertEqual "(1, 1) should be equal to (1, 1)"
      (compare (Pos 1 1) (Pos 1 1)) (EQ)
    assertEqual "(1, 1) should be lt (1, 2)"
      (compare (Pos 1 1) (Pos 1 2)) (LT)
    assertEqual "(3, 4) should be lt (4, 4)"
      (compare (Pos 3 4) (Pos 4 4)) (LT)
    assertEqual "(5, 7) should be gt (2, 12)"
      (compare (Pos 5 7) (Pos 2 12)) (GT)

testBlockEq = TestCase $ do
    let b1 = Block (Pos 1 1) (Pos 2 1)
    let b2 = Block (Pos 2 1) (Pos 2 2)
    assertBool "b1 should equal itself" (b1 == b1)
    assertBool "b2 should equal itself" (b2 == b2)
    assertBool "b1 should not equal b2" (b1 /= b2)


testBlockOrd = TestCase $ do
    let b1 = Block (Pos 1 1) (Pos 2 1)
        b2 = Block (Pos 1 1) (Pos 2 2)   -- not a valid block, but ok for test
    assertEqual "b1 should equal itself" (b1 `compare` b1) (EQ)
    assertEqual "b2 should equal itself" (b2 `compare` b2) (EQ)
    assertEqual "b1 should be lt b2" (b1 `compare` b2) (LT)
    assertEqual "b2 should be gt b1" (b2 `compare` b1) (GT)

testBlockStanding = TestCase $ do
    let b1 = Block (Pos 1 1) (Pos 1 1)
        b2 = Block (Pos 1 1) (Pos 2 1)
    assertBool "b1 is standing" (isStanding b1)
    assertBool "b2 is not standing" (not $ isStanding b2)               

testMoves = TestCase $ do
    let b1 = Block (Pos 3 3) (Pos 3 3)  -- standing block
        b2 = Block (Pos 3 3) (Pos 3 4)  -- row eq block
        b3 = Block (Pos 3 4) (Pos 4 4)  -- col eq block
    assertBool "b1 is standing" (isStanding b1)
    assertBool "b2 is not standing" (not $ isStanding b2)
    assertBool "b3 is not standing" (not $ isStanding b3)
    assertEqual "standing move left test" 
       (Block (Pos 3 1) (Pos 3 2)) (applyMove b1 LeftMove)
    assertEqual "standing move right test" 
       (Block (Pos 3 4) (Pos 3 5)) (applyMove b1 RightMove)
    assertEqual "standing move up test" 
       (Block (Pos 1 3) (Pos 2 3)) (applyMove b1 UpMove)
    assertEqual "standing move down test"
       (Block (Pos 4 3) (Pos 5 3)) (applyMove b1 DownMove)
    assertEqual "row equal move left test" 
       (Block (Pos 3 2) (Pos 3 2)) (applyMove b2 LeftMove)
    assertEqual "row equal move right test"
       (Block (Pos 3 5) (Pos 3 5)) (applyMove b2 RightMove)
    assertEqual "row equal move up test" 
       (Block (Pos 2 3) (Pos 2 4)) (applyMove b2 UpMove)
    assertEqual "row equal move down test"
       (Block (Pos 4 3) (Pos 4 4))  (applyMove b2 DownMove)
    assertEqual "col equal move left test" 
       (Block (Pos 3 3) (Pos 4 3)) (applyMove b3 LeftMove)
    assertEqual "col equal move right test"
       (Block (Pos 3 5) (Pos 4 5)) (applyMove b3 RightMove)
    assertEqual "col equal move up test" 
       (Block (Pos 2 4) (Pos 2 4)) (applyMove b3 UpMove)
    assertEqual "col equal move down test"
       (Block (Pos 5 4) (Pos 5 4))  (applyMove b3 DownMove)

tests = TestList [TestLabel "test Pos Eq" testPosEq,
                  TestLabel "test Pos Ord" testPosOrd,
                  TestLabel "test Block Eq" testBlockEq,
                  TestLabel "test Block Ord" testBlockOrd,
                  TestLabel "test Block standing" testBlockStanding,
                  TestLabel "test Block moves" testMoves]

main = runTestTT tests