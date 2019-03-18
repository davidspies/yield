{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.ST.Class
  ( MonadST(..)
  )
where

import           DSpies.Prelude

import           Control.Monad.Cont             ( ContT )
import           Control.Monad.ST               ( ST )

class Monad m => MonadST m where
  type World m
  liftST :: ST (World m) a -> m a

instance (MonadTrans t, MonadST m, Monad (t m))
    => MonadST (Transformed t m) where
  type World (Transformed t m) = World m
  liftST = lift . liftST
deriving via (Transformed (ReaderT r) m) instance MonadST m
  => MonadST (ReaderT r m)
deriving via (Transformed (WriterT w) m) instance (Monoid w, MonadST m)
  => MonadST (WriterT w m)
deriving via (Transformed (StateT s) m) instance MonadST m
  => MonadST (StateT s m)
deriving via (Transformed (ExceptT e) m) instance MonadST m
  => MonadST (ExceptT e m)
deriving via (Transformed (ContT r) m) instance MonadST m
  => MonadST (ContT r m)
