{-# LANGUAGE Safe #-}

module Control.Monad.Yield
  ( Yield,
    runYield,
    yield,
  )
where

import Control.Monad
import Control.Monad.Yield.Class
import Prelude

data Yield a b = Continue (a, Yield a b) | Final b
  deriving stock (Functor)

instance Applicative (Yield a) where
  pure = Final
  (<*>) = ap

instance Monad (Yield a) where
  (>>=) (Final x) fn = fn x
  (>>=) (Continue (x, y)) fn = Continue (x, y >>= fn)

instance MonadYield a (Yield a) where
  yield x = Continue (x, Final ())

runYield :: Yield a b -> [a]
runYield = \case
  Continue (x, y) -> x : runYield y
  Final _ -> []
