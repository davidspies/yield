module Control.Monad.Yield.ST
  ( YieldST
  , liftST
  , runYieldST
  , yield
  )
where

import           DSpies.Prelude

import qualified Control.Monad.Reader          as Reader
import           Control.Monad.ST
import           Data.Constraint                ( Dict(..) )
import           System.IO.Unsafe               ( unsafePerformIO )

import           Control.Monad.ST.Class         ( MonadST(..) )
import           Control.Monad.Yield.Class
import           Data.MemoRef

newtype Yielder s a b = Yielder (MemoRef (Either (a, Yielder s a b) b))

pureY :: b -> IO (Yielder s a b)
pureY v = Yielder <$> pureMemoRef (Right v)

bindY :: Yielder s a b -> (b -> IO (Yielder s a c)) -> IO (Yielder s a c)
bindY (Yielder actRef) fn =
  fmap Yielder . newMemoRef $ readMemoRef actRef >>= \case
    Left  (nextV, nextY) -> Left . (nextV, ) <$> bindY nextY fn
    Right val            -> do
      Yielder resRef <- fn val
      readMemoRef resRef

newtype YieldST s a b
  = YieldST {unYieldIOM :: ReaderT (Dict (s ~ RealWorld)) IO (Yielder s a b)}

instance Functor (YieldST s a) where
  fmap = liftM
instance Applicative (YieldST s a) where
  pure  = YieldST . lift . pureY
  (<*>) = ap
instance Monad (YieldST s a) where
  (>>=) (YieldST mkX) fn = YieldST $ do
    x    <- mkX
    dict <- Reader.ask
    lift $ x `bindY` (\v -> runReaderT (unYieldIOM $ fn v) dict)

instance MonadYield a (YieldST s a) where
  yield x = YieldST $ lift $ Yielder <$> newMemoRef
    (Left . (x, ) . Yielder <$> pureMemoRef (Right ()))

runYieldST :: (forall s . YieldST s a b) -> [a]
runYieldST (YieldST act) = unsafePerformIO $ go =<< runReaderT act Dict
 where
  go :: Yielder RealWorld a b -> IO [a]
  go (Yielder r) = readMemoRef r <&> \case
    Left  (h, t) -> h : unsafePerformIO (go t)
    Right _      -> []

instance MonadST s (YieldST s a) where
  liftST act = YieldST $ do
    Dict <- Reader.ask
    lift $ Yielder <$> newMemoRef (stToIO $ Right <$> act)
