module OrTests (or_tests) where

import Test.HUnit
import Control.Applicative
import FPInScala.ErrorHandling.Or

type OrSI = Or String Int

sqfunc :: Int -> Int
sqfunc x = x * x

monadfunc :: Int -> OrSI
monadfunc x | x > 0 = Second (x+1)
            | otherwise = First "Non-positive"

test_functor = TestCase $ do
    assertEqual "fmap should return First on First"
        (First "Empty" :: OrSI) (fmap sqfunc (First "Empty" :: OrSI))
    assertEqual "fmap should work on Second"
        (Second 25 :: OrSI) (fmap sqfunc $ Second 5 :: OrSI)

test_applicative = TestCase $ do
    assertEqual "pure should wrap in Second"
        (Second 4 :: OrSI) (pure 4 :: OrSI)
    assertEqual "<*> should apply a function"
        (Second 4 :: OrSI) (Second sqfunc <*> Second 2 :: OrSI)
    assertEqual "<*> should respect First"
        (First "Empty") ((First "Empty") <*> Second 3 :: OrSI)

test_monad = TestCase $ do
    assertEqual ">>= should chain computations"
        (Second 4 :: OrSI) ((Second 3 :: OrSI) >>= monadfunc)
    assertEqual ">>= should respect func None in chain"
        (First "Non-positive" :: OrSI) ((Second 0 :: OrSI) >>= monadfunc)
    assertEqual ">>= should respect left First in chain"
        (First "other") ((First "other" :: OrSI) >>= monadfunc)
    assertEqual ">> should discard non-First left"
        (Second 2 :: OrSI) (Second 4 >> Second 2)
    assertEqual ">> should respect left First"
        (First "other") ((First "other") >> Second 2)
    assertEqual "return should wrap in Second"
        (Second 7 :: OrSI) (return 7 :: OrSI)

test_orelse = TestCase $ do
    assertEqual "orElse should give argument when given non-First"
        (Second 3) (orElse (Second 3) (First "else"))
    assertEqual "orElse should give argument when given non-First"
        (Second 11 :: OrSI) (orElse (Second 11) (Second 2))

test_orsequence = TestCase $ do
    assertEqual "orSequence should return First if any First values"
        (First "error") (orSequence [Second 3, Second 2, First "error", Second 9])
    assertEqual "orSequence should return concatenated list if no First values"
        (Second [3, 2, 9]) (orSequence ([Second 3, Second 2, Second 9] :: [OrSI]))

test_ortraverse = TestCase $ do
    assertEqual "orTraverse should evaluate to error message if any errors encountered"
        (First "Non-positive") (orTraverse monadfunc [1, 2, 3, 4, -1, 4])
    assertEqual "orTraverse should return as Second if no Firsts are encountered"
        (Second [2, 3, 4, 12]) (orTraverse monadfunc [1, 2, 3, 11])

or_tests = [TestLabel "test Or functor" test_functor,
            TestLabel "test Or applicative" test_applicative,
            TestLabel "test Or monad" test_monad,
            TestLabel "test Or orElse" test_orelse,
            TestLabel "test orSequence" test_orsequence,
            TestLabel "test orTraverse" test_ortraverse]
