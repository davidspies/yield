module Data.MemoRef
  ( MemoRef
  , newMemoRef
  , pureMemoRef
  , readMemoRef
  )
where

import           DSpies.Prelude

import           Control.Concurrent.STM
import           Control.Monad.Fix              ( mfix )

-- |
-- A box which holds an IO action and lazily evaluates it only if and when the
-- value is needed.
newtype MemoRef a = MemoRef (TVar (IO a))

-- |
-- Equivalent to
-- @ 'newMemoRef' . 'return' @
pureMemoRef :: a -> IO (MemoRef a)
pureMemoRef = fmap MemoRef . newTVarIO . return

-- |
-- Will execute the action at most once when and if 'readMemoRef' is called
-- on the returned 'MemoRef'.
-- Subsequent calls to 'readMemoRef' just return the memoized result.
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

-- |
-- Read from a 'MemoRef' (see 'newMemoRef')
--
-- Note: This function is thread-safe. If multiple threads try
-- to read from a 'MemoRef' simultaneously, all but the first will block until
-- the action finishes (and then return the memoized result).
readMemoRef :: MemoRef a -> IO a
readMemoRef (MemoRef actRef) = join $ readTVarIO actRef
