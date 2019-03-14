module MemoRef
  ( MemoRef
  , newMemoRef
  , pureRef
  , readMemoRef
  )
where

import           Control.Monad                  ( join )
import           Control.Monad.Fix              ( mfix )
import           Control.Monad.ST
import           Data.STRef

data MemoRef s a = PureValue a | MemoRef (STRef s (ST s a))

pureRef :: a -> MemoRef s a
pureRef = PureValue

newMemoRef :: ST s a -> ST s (MemoRef s a)
newMemoRef act = fmap MemoRef $ mfix $ \r -> newSTRef $ do
  result <- act
  writeSTRef r (return result)
  return result

readMemoRef :: MemoRef s a -> ST s a
readMemoRef = \case
  PureValue x      -> return x
  MemoRef   actRef -> join $ readSTRef actRef
