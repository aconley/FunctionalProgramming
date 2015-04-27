module Main (main) where

import Test.HUnit
import Huffman

-- some very simple tests
 
testb = TestCase $ do
  let h_exp = "Fork(Leaf(h, 1) , Leaf(e, 2), he, 3)"
      h_build = show $ createHuffmanTree "hee"
  assertEqual "Tree built from 'hee' should show as expected" h_exp h_build

test_encode = TestCase $ do
  let huff = createHuffmanTree "hello"
      exp_code = Just [1, 1, 1, 1, 1, 0, 0, 0]
  assertEqual "hell should decode as expected in hello tree" 
    exp_code (encode huff "hell")
  assertEqual "Shouldn't be able to encode fudge in hello tree"
    Nothing (encode huff "fudge")

test_decode = TestCase $ do
  let huff = createHuffmanTree "hello"
      code = [1, 1, 1, 1, 1, 0, 0, 0]
  assertEqual "Should get hell from code" (Just "hell") (decode huff code)
  assertEqual "Should get failure from code [1,0,1]" 
    Nothing (decode huff [1, 0, 1])

test_mapencode = TestCase $ do
  let huff = createHuffmanTree "hello"
      hmap = createHuffmanMap huff
      exp_code = Just [1, 1, 1, 1, 1, 0, 0, 0]
  assertEqual  "hell should decode as expected in hello map" 
    exp_code (quickEncode hmap "hell")
  assertEqual "Shouldn't be able to encode fudge in hello map"
    Nothing (quickEncode hmap "fudge")

                      
tests = TestList [TestLabel "test tree building" testb,
                  TestLabel "test encoding" test_encode,
                  TestLabel "test decoding" test_decode,
                  TestLabel "test quick encoding" test_mapencode]

main = runTestTT tests
