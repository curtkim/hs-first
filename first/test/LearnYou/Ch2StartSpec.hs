module LearnYou.Ch2StartSpec where

import Test.Hspec

spec :: Spec
spec = do

  describe "List" $ do
    it "++" $ do 
      [1,2,3,4] ++ [9,10,11,12] `shouldBe` [1,2,3,4,9,10,11,12] 
