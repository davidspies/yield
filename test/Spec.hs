import           Control.Monad
import           Data.STRef
import           Test.Hspec

import           Control.Monad.YieldST

main :: IO ()
main = hspec $ describe "YieldST" $ do
  it "should reach the end of the list"
    $          runYieldST (countTo 100)
    `shouldBe` [1 .. 100]
  it "should lazily produce values"
    $          take 100 (runYieldST countForever)
    `shouldBe` [1 .. 100]

countTo :: Int -> YieldST s Int ()
countTo n = do
  ref <- inST $ newSTRef 0
  replicateM_ n $ do
    i <- inST $ do
      modifySTRef ref (+ 1)
      readSTRef ref
    yield i

countForever :: YieldST s Int ()
countForever = do
  ref <- inST $ newSTRef 0
  forever $ do
    i <- inST $ do
      modifySTRef ref (+ 1)
      readSTRef ref
    yield i
