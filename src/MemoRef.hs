module MemoRef
  ( MemoRef
  , newMemoRef
  , pureRef
  , readMemoRef
  )
where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Fix              ( mfix )

data MemoRef a = PureValue a | MemoRef (TVar (IO a))

pureRef :: a -> MemoRef a
pureRef = PureValue

newMemoRef :: IO a -> IO (MemoRef a)
newMemoRef act = fmap MemoRef $ do
  begunEvaluation <- newTVarIO False
  resultBox       <- newEmptyTMVarIO
  mfix $ \r -> newTVarIO $ do
    hasBegun <- atomically $ do
      res <- readTVar begunEvaluation
      unless res $ writeTVar begunEvaluation True
      return res
    if hasBegun
      then atomically $ readTMVar resultBox
      else do
        result <- act
        atomically $ do
          putTMVar resultBox result
          writeTVar r (return result)
        return result

readMemoRef :: MemoRef a -> IO a
readMemoRef = \case
  PureValue x      -> return x
  MemoRef   actRef -> join $ readTVarIO actRef
