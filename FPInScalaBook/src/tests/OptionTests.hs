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


option_tests = [TestLabel "test functor" test_functor,
                TestLabel "test applicative" test_applicative,
                TestLabel "test monad" test_monad,
                TestLabel "test getOrElse" test_optionfuncs]
