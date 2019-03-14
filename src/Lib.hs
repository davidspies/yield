module Lib
  ( xs
  , module X
  )
where

import           Data.STRef

import           YieldST                       as X

xs :: [Int]
xs = runYieldST $ do
  r <- inST $ newSTRef 1
  yield =<< inST (readSTRef r)
  inST $ modifySTRef r (+ 1)
  yield =<< inST (readSTRef r)
  undefined
