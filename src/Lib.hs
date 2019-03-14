module Lib
  ( YieldST
  , inST
  , runYieldST
  , yield
  )
where

import           Control.Monad
import           Control.Monad.ST
import           System.IO.Unsafe               ( unsafePerformIO )

import           MemoRef

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

newtype Yielder s a b = Yielder (MemoRef s (Either (a, Yielder s a b) b))

pureY :: ST s b -> ST s (Yielder s a b)
pureY v = Yielder <$> newMemoRef (Right <$> v)

bindY :: Yielder s a b -> (b -> ST s (Yielder s a c)) -> ST s (Yielder s a c)
bindY (Yielder actRef) fn =
  fmap Yielder . newMemoRef $ readMemoRef actRef >>= \case
    Left  (nextV, nextY) -> Left . (nextV, ) <$> bindY nextY fn
    Right val            -> do
      Yielder resRef <- fn val
      readMemoRef resRef

newtype YieldST s a b = YieldST {unYieldIOM :: ST s (Yielder s a b)}

instance Functor (YieldST s a) where
  fmap = liftM
instance Applicative (YieldST s a) where
  pure  = YieldST . pureY . return
  (<*>) = ap
instance Monad (YieldST s a) where
  (>>=) (YieldST x) fn = YieldST $ x >>= (`bindY` (unYieldIOM . fn))

yield :: a -> YieldST s a ()
yield x = YieldST $ Yielder <$> newMemoRef
  (pure $ Left (x, Yielder $ pureRef $ Right ()))

runYieldST :: (forall s . YieldST s a b) -> [a]
runYieldST (YieldST act) = unsafePerformIO $ stToIO $ go =<< act
 where
  go :: Yielder RealWorld a b -> ST RealWorld [a]
  go (Yielder r) = readMemoRef r <&> \case
    Left  (h, t) -> h : unsafePerformIO (stToIO $ go t)
    Right _      -> []

inST :: ST s b -> YieldST s a b
inST act = YieldST $ Yielder <$> newMemoRef (Right <$> act)
