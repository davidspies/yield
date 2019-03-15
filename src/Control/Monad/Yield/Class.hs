module Control.Monad.Yield.Class
    ( MonadYield(..)
    ) where

import           DSpies.Prelude

class Monad m => MonadYield a m | m -> a where
  yield :: a -> m ()
