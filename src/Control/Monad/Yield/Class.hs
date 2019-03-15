{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Yield.Class
  ( MonadYield(..)
  )
where

import           DSpies.Prelude

import           Control.Monad.Cont             ( ContT )

class Monad m => MonadYield a m | m -> a where
  yield :: a -> m ()

instance (MonadTrans t, MonadYield a m, Monad (t m))
    => MonadYield a (Transformed t m) where
  yield = lift . yield
deriving via (Transformed (ReaderT r) m) instance MonadYield a m
  => MonadYield a (ReaderT r m)
deriving via (Transformed (WriterT w) m) instance (Monoid w, MonadYield a m)
  => MonadYield a (WriterT w m)
deriving via (Transformed (ExceptT e) m) instance MonadYield a m
  => MonadYield a (ExceptT e m)
deriving via (Transformed (StateT s) m) instance MonadYield a m
  => MonadYield a (StateT s m)
deriving via (Transformed (ContT c) m) instance MonadYield a m
  => MonadYield a (ContT c m)
