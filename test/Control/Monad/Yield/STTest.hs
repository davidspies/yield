module Control.Monad.Yield.STTest where

import           DSpies.Prelude

import           Control.Monad.Primitive
import           Data.STRef
import           Test.Hspec

import           Control.Monad.Yield.ST

spec_yieldst :: Spec
spec_yieldst = describe "YieldST" $ do
  it "should reach the end of the list"
    $          runYieldST (countToST 10)
    `shouldBe` [1 .. 10]
  it "should lazily produce values"
    $          take 10 (runYieldST (countToST 10 >> error "fail"))
    `shouldBe` [1 .. 10]

countToST :: Int -> YieldST s Int ()
countToST n = do
  ref <- stToPrim $ newSTRef 0
  replicateM_ n $ do
    i <- stToPrim $ do
      modifySTRef ref (+ 1)
      readSTRef ref
    yield i
