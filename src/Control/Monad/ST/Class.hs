{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.ST.Class
  ( MonadST(..)
  )
where

import           DSpies.Prelude

import           Control.Monad.Cont             ( ContT )
import           Control.Monad.ST               ( ST )

class Monad m => MonadST s m | m -> s where
  liftST :: ST s a -> m a

instance (MonadTrans t, MonadST s m, Monad (t m))
    => MonadST s (Transformed t m) where
  liftST = lift . liftST
deriving via (Transformed (ReaderT r) m) instance MonadST s m
  => MonadST s (ReaderT r m)
deriving via (Transformed (WriterT w) m) instance (Monoid w, MonadST s m)
  => MonadST s (WriterT w m)
deriving via (Transformed (StateT s') m) instance MonadST s m
  => MonadST s (StateT s' m)
deriving via (Transformed (ExceptT e) m) instance MonadST s m
  => MonadST s (ExceptT e m)
deriving via (Transformed (ContT r) m) instance MonadST s m
  => MonadST s (ContT r m)
