module OptionTests (option_tests) where

import Test.HUnit
import Control.Applicative
import FPInScala.ErrorHandling.Option
import FPInScala.ErrorHandling.OptionExamples

sqfunc :: Int -> Int
sqfunc x = x * x

monadfunc :: Int -> Option Int
monadfunc x | x > 0 = Some (x+1)
            | otherwise = None

test_functor = TestCase $ do
    assertEqual "fmap should return None on None"
        None (fmap sqfunc None)
    assertEqual "fmap should work on Some"
        (Some 25) (fmap sqfunc $ Some 5)

test_applicative = TestCase $ do
    assertEqual "pure should wrap in Some"
        (Some 'y') (pure 'y')
    assertEqual "<*> should apply a function"
        (Some 4) (Some sqfunc <*> Some 2)
    assertEqual "<*> should respect None"
        None (None <*> Some 'a' :: Option Char)

test_monad = TestCase $ do
    assertEqual ">>= should chain computations"
        (Some 4) (Some 3 >>= monadfunc)
    assertEqual ">>= should respect func None in chain"
        None (Some 0 >>= monadfunc)
    assertEqual ">>= should respect left None in chain"
        None (None >>= monadfunc)
    assertEqual ">> should discard non-None left"
        (Some 2) (Some 4 >> Some 2)
    assertEqual ">> should respect left None"
        (None) (None >> Some 2)
    assertEqual "return should wrap in Some"
        (Some 'a') (return 'a')

test_optionfuncs = TestCase $ do
    assertEqual "getOrElse should get some"
        ('f') (getOrElse (Some 'f') 'a')
    assertEqual "getOrElse should get default when given None"
        ('k') (getOrElse None 'k')
    assertEqual "orElse should give argument when given non-None"
        (Some 'x') (orElse (Some 'x') None)
    assertEqual "orElse should give argument when given non-None"
        (Some 'x') (orElse (Some 'x') (Some 'k'))

test_optmean = TestCase $ do
    assertEqual "optMean should return None for empty list"
        None (optMean [])
    assertEqual "optMean should work on non-empty list"
        (Some 2.5) (optMean [1.0, 2.0, 3.0, 4.0])

test_optvar = TestCase $ do
    assertEqual "optVariance should return None for empty list"
        None (optVariance [])
    assertEqual "optVariance should work on non-empty lists"
        (Some 1.25) (optVariance [1.0, 2.0, 3.0, 4.0])

test_optabs = TestCase $ do
    assertEqual "optAbs should return None for empty list"
        None (optAbs None)
    assertEqual "optAbs should work on actual value"
        (Some 4) (optAbs (Some (-4)))

test_optsequence = TestCase $ do
    assertEqual "optSequence should return None if any None values"
        None (optSequence [Some 'a', Some 'b', None, Some 'c'])
    assertEqual "optSequence should return Some of concatenated list if no Nones"
        (Some ['a', 'b', 'c']) (optSequence [Some 'a', Some 'b', Some 'c'])

test_opttraverse = TestCase $ do
    assertEqual "optTraverse should evaluate to None if any Nones encountered"
        None (optTraverse monadfunc [1, 2, 3, 4, -1, 4])
    assertEqual "optTraverse should return as Some if no Nones are encountered"
        (Some [2, 3, 4, 12]) (optTraverse monadfunc [1, 2, 3, 11])

test_optseqtraverse = TestCase $ do
    assertEqual "optSeqTraverse should return None if any None values"
        None (optSeqTraverse [Some 'a', Some 'b', None, Some 'c'])
    assertEqual "optSeqTraverse should return Some of concatenated list if no Nones"
        (Some ['a', 'b', 'c']) (optSeqTraverse [Some 'a', Some 'b', Some 'c'])

option_tests = [TestLabel "test functor" test_functor,
                TestLabel "test applicative" test_applicative,
                TestLabel "test monad" test_monad,
                TestLabel "test getOrElse" test_optionfuncs,
                TestLabel "test optMean" test_optmean,
                TestLabel "test optVar" test_optvar,
                TestLabel "test optAbs" test_optabs,
                TestLabel "test optSequence" test_optsequence,
                TestLabel "test optTraverse" test_opttraverse,
                TestLabel "test optSeqTraverse" test_optseqtraverse]
