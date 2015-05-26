module FPInScala.ErrorHandling.Option (
    Option(..),
    getOrElse,
    orElse,
    optionFilter,
    lift,
    map2,
    map2Alt,
    optSequence,
    optTraverse,
    optSeqTraverse
    ) where

import Control.Applicative

-- A number of the things we implement here are already
--  in the prelude so we give them 'ugly' names like
--  optFilter to avoid just piggybacking off the built
--  in versions that come from making Option an instance
--  of various typeclasses

-- Maybe in Prelude
data Option a = None | Some a deriving (Show, Eq)

instance Functor Option where
    fmap _ None = None
    fmap f (Some x) = Some (f x)

instance Applicative Option where
    pure = Some

    Some f <*> m = fmap f m
    None <*> _ = None

instance Monad Option where
    Some x >>= f = f x
    None >>= _ = None

    Some _ >> k = k
    None >> _ = None

    return = Some
    fail _ = None

getOrElse :: Option a -> a -> a
getOrElse (Some x) _ = x
getOrElse None d = d

orElse :: Option a -> Option a -> Option a
orElse None d = d
orElse (Some x) _ = Some x

optionFilter :: Option a -> (a -> Bool) -> Option a
optionFilter (Some x) f | f x == True = Some x
                           | otherwise = None

lift :: (a -> b) -> Option a -> Option b
lift f = fmap f

map2 :: (a -> b -> c) -> Option a -> Option b -> Option c
map2 f x y = x >>= g
    where g z = fmap (\xx -> f z xx) y

map2Alt :: (a -> b -> c) -> Option a -> Option b -> Option c
map2Alt _ None _ = None
map2Alt _ _ None = None
map2Alt f (Some x) (Some y) = Some $ f x y

-- Note there's a built in version in Prelude
optSequence :: [Option a] -> Option [a]
optSequence [] = Some [] :: Option [a]
optSequence (x:xs) = x >>= g
    where g y = fmap (\z -> y:z) $ optSequence xs

-- Also a built in Prelude version
optTraverse :: (a -> Option b) -> [a] -> Option [b]
optTraverse _ [] = Some [] :: Option [b]
optTraverse f (x:xs) = map2 g (f x) (optTraverse f xs)
    where g p q = p : q

-- Sequence in terms of traverse
optSeqTraverse :: [Option a] -> Option [a]
optSeqTraverse = optTraverse id
