module Data.MemoRef
  ( MemoRef
  , newMemoRef
  , pureMemoRef
  , readMemoRef
  )
where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Fix              ( mfix )

newtype MemoRef a = MemoRef (TVar (IO a))

-- | Equivalent to @ newMemoRef . return @
pureMemoRef :: a -> IO (MemoRef a)
pureMemoRef = fmap MemoRef . newTVarIO . return

newMemoRef :: IO a -> IO (MemoRef a)
newMemoRef act = fmap MemoRef $ do
  begunEvaluation <- newTVarIO False
  resultBox       <- newEmptyTMVarIO
  mfix $ \r -> newTVarIO $ do
    hasBegun <- atomically $ do
      hasBegun <- readTVar begunEvaluation
      unless hasBegun $ writeTVar begunEvaluation True
      return hasBegun
    if hasBegun
      then atomically $ readTMVar resultBox
      else do
        result <- act
        atomically $ do
          putTMVar resultBox result
          writeTVar r (return result)
        return result

readMemoRef :: MemoRef a -> IO a
readMemoRef (MemoRef actRef) = join $ readTVarIO actRef
