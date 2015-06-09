module FPInScala.State.StateMonad (
    State(..)
    ) where

import Control.Applicative

-- Full fledged State.  Note: Haskell already has Control.Monad.State
data State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State h) = State $ \s -> let (a, newState) = h s
                                     in (f a, newState)

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

