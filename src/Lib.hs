module Lib
  ( YieldST
  , inST
  , runYieldST
  , yield
  )
where

import           Control.Monad
import           Control.Monad.Reader           ( ReaderT(..)
                                                , lift
                                                )
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.ST
import           Data.Constraint                ( Dict(..) )
import           System.IO.Unsafe               ( unsafePerformIO )

import           MemoRef

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

newtype Yielder s a b = Yielder (MemoRef (Either (a, Yielder s a b) b))

pureY :: b -> Yielder s a b
pureY v = Yielder $ pureRef (Right v)

bindY :: Yielder s a b -> (b -> IO (Yielder s a c)) -> IO (Yielder s a c)
bindY (Yielder actRef) fn =
  fmap Yielder . newMemoRef $ readMemoRef actRef >>= \case
    Left  (nextV, nextY) -> Left . (nextV, ) <$> bindY nextY fn
    Right val            -> do
      Yielder resRef <- fn val
      readMemoRef resRef

newtype YieldST s a b = YieldST {unYieldIOM :: ReaderT (Dict (s ~ RealWorld)) IO (Yielder s a b)}

instance Functor (YieldST s a) where
  fmap = liftM
instance Applicative (YieldST s a) where
  pure  = YieldST . pure . pureY
  (<*>) = ap
instance Monad (YieldST s a) where
  (>>=) (YieldST mkX) fn = YieldST $ do
    x    <- mkX
    dict <- Reader.ask
    lift $ x `bindY` (\v -> runReaderT (unYieldIOM $ fn v) dict)

yield :: a -> YieldST s a ()
yield x = YieldST $ lift $ Yielder <$> newMemoRef
  (pure $ Left (x, Yielder $ pureRef $ Right ()))

runYieldST :: (forall s . YieldST s a b) -> [a]
runYieldST (YieldST act) = unsafePerformIO $ go =<< runReaderT act Dict
 where
  go :: Yielder RealWorld a b -> IO [a]
  go (Yielder r) = readMemoRef r <&> \case
    Left  (h, t) -> h : unsafePerformIO (go t)
    Right _      -> []

inST :: ST s b -> YieldST s a b
inST act = YieldST $ do
  Dict <- Reader.ask
  lift $ Yielder <$> newMemoRef (stToIO $ Right <$> act)
