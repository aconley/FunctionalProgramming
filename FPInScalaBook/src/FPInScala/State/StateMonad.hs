module FPInScala.State.StateMonad (
    State(..),
    mapS2,
    sequenceS
    ) where

import Control.Applicative

-- Full fledged State.  Note: Haskell already has Control.Monad.State
data State s a = State { runState :: s -> (a, s) }

-- map from the book
instance Functor (State s) where
    fmap f (State h) = State $ \s -> let (a, newState) = h s
                                     in (f a, newState)

-- return is unit, >>= is flatmap
instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s
                                      (State g) = f a
                                  in g newState

instance Applicative (State s) where
  pure = return
  f <*> x = do
    f' <- f
    x' <- x
    return (f' x')

mapS2 :: (a -> b -> c) -> State s a -> State s b -> State s c
mapS2 f sa sb = sa >>= \x -> fmap (\y -> f x y) sb

-- try it with a fold this time
sequenceS :: [State s a] -> State s [a]
sequenceS sas = foldr f (return []) sas
  where f v acc = mapS2 (:) v acc
