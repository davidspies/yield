module Control.Monad.YieldTest where

import           DSpies.Prelude

import qualified Control.Monad.State           as State
import           Test.Hspec

import           Control.Monad.Yield

spec_yield :: Spec
spec_yield = describe "Yield" $ do
  it "should reach the end of the list"
    $          runYield (countTo 10)
    `shouldBe` [1 .. 10]
  it "should lazily produce values"
    $          take 10 (runYield (countTo 10 >> error "fail"))
    `shouldBe` [1 .. 10]

countTo :: Int -> Yield Int ()
countTo n = void $ (`runStateT` 0) $ replicateM_ n $ do
  State.modify (+ 1)
  i <- State.get
  lift $ yield i
