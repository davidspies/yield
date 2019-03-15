import           Control.Monad
import           Control.Monad.State            ( lift
                                                , runStateT
                                                )
import qualified Control.Monad.State           as State
import           Data.STRef
import           Test.Hspec

import           Control.Monad.Yield
import           Control.Monad.Yield.ST

main :: IO ()
main = hspec $ do
  describe "Yield" $ do
    it "should reach the end of the list"
      $          runYield (countTo 10)
      `shouldBe` [1 .. 10]
    it "should lazily produce values"
      $          take 10 (runYield (countTo 10 >> error "fail"))
      `shouldBe` [1 .. 10]
  describe "YieldST" $ do
    it "should reach the end of the list"
      $          runYieldST (countToST 10)
      `shouldBe` [1 .. 10]
    it "should lazily produce values"
      $          take 10 (runYieldST (countToST 10 >> error "fail"))
      `shouldBe` [1 .. 10]

countTo :: Int -> Yield Int ()
countTo n = void $ (`runStateT` 0) $ replicateM_ n $ do
  State.modify (+ 1)
  i <- State.get
  lift $ yield i

countToST :: Int -> YieldST s Int ()
countToST n = do
  ref <- inST $ newSTRef 0
  replicateM_ n $ do
    i <- inST $ do
      modifySTRef ref (+ 1)
      readSTRef ref
    yield i
