module TreeTests (tree_tests) where

import Test.HUnit
import FPInScala.DataStructures.Tree

t1 = Leaf 4 :: Tree Int
t2 = Branch (Leaf 3) (Leaf 2) :: Tree Int
t3 = Branch (Branch (Leaf 1) (Leaf 7)) (Leaf 11)
t4 = Branch (Leaf 10) (Branch (Branch (Leaf 2) (Leaf 10)) (Branch (Leaf 2) (t3)))

test_size = TestCase $ do
    assertEqual "size of single leaf should be 1" (size t1) 1
    assertEqual "size of t2 should be 3" 3 (size t2)
    assertEqual "size of t3 should be 5" 5 (size t3)
    assertEqual "size of t4 should be 13" 13 (size t4)

test_treemax = TestCase $ do
    assertEqual "max of t1 should be 4" 4 (treeMax t1)
    assertEqual "max of t2 should be 3" 3 (treeMax t2)
    assertEqual "max of t3 should be 11" 11 (treeMax t3)
    assertEqual "max of t4 should be 11" 11 (treeMax t4)

test_depth = TestCase $ do
    assertEqual "depth of t1 should be 1" 1 (depth t1)
    assertEqual "depth of t2 should be 2" 2 (depth t2)
    assertEqual "depth of t3 should be 3" 3 (depth t3)
    assertEqual "depth of t4 should be 6" 6 (depth t4)

tree_tests = [TestLabel "test size" test_size,
              TestLabel "test treeMax" test_treemax,
              TestLabel "test depth" test_depth]
