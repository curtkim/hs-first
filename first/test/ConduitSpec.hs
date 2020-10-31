module ConduitSpec where

import Test.Hspec

import Data.Conduit
import qualified Data.Conduit.List as L

import Conduit


spec :: Spec
spec = do

  it "first" $ do
    (runConduitPure $ L.sourceList [1 .. 5] .| L.fold (+) 0) `shouldBe` 15
    (runConduitPure $ yieldMany [1..10] .| sumC) `shouldBe` 55