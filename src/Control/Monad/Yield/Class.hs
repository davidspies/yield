module Control.Monad.Yield.Class
    ( MonadYield(..)
    ) where

class Monad m => MonadYield a m | m -> a where
  yield :: a -> m ()
