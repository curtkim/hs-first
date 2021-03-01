module LionTraverseSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

evenFilter :: Integral a => a -> Maybe a
evenFilter a = if (even a)
                then Just (a*a)
                else Nothing

spec :: Spec
spec = do

  describe "Traverse" $ do
    it "map vs traverse" $ do
      evenFilter 2 `shouldBe` Just 4
      map evenFilter [2,4,6] `shouldBe` [Just 4, Just 16, Just 36]
      map evenFilter [1,2,3] `shouldBe` [Nothing, Just 4, Nothing]

      traverse evenFilter [2,4,6] `shouldBe` Just [4,16,36]
      traverse evenFilter [1,2,3] `shouldBe` Nothing

