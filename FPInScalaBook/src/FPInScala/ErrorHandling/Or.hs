module FPInScala.ErrorHandling.Or (
    Or(..),
    orElse,
    map2,
    orSequence,
    orTraverse
    ) where

import Control.Applicative

-- Use the name Or instead of Either, which is a built-in in Haskell
--  and First for Left, Second for Right

-- Either in Prelude
data Or a b = First a | Second b deriving (Show, Eq)

instance Functor (Or a) where
    fmap _ (First x) = (First x)
    fmap f (Second x) = Second (f x)

instance Applicative (Or a) where
    pure = Second

    Second f <*> m = fmap f m
    First x <*> _ = First x

instance Monad (Or a) where
    Second x >>= f = f x
    First x >>= _ = First x

    return = Second

-- Return whichever argument is not First
orElse :: Or a b -> Or a b -> Or a b
orElse (First _) d = d
orElse (Second x) _ = Second x

map2 :: (a -> b -> c) -> Or e a -> Or e b -> Or e c
map2 _ (First x) _ = First x
map2 _ _ (First x) = First x
map2 f (Second x) (Second y) = Second $ f x y

-- Built in Prelude version of traverse, so use different name
orTraverse :: (a -> Or e b) -> [a] -> Or e [b]
orTraverse _ [] = Second [] :: Or e [b]
orTraverse f (x:xs) = map2 g (f x) (orTraverse f xs)
    where g p q = p : q

orSequence :: [Or e a] -> Or e [a]
orSequence = orTraverse id
