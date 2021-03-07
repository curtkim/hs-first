module LearnYou.Ch1IntroSpec where

import Test.Hspec

spec :: Spec
spec = do

  describe "Expectation" $ do
    it "shouldStartWith" $ do
      [1,2] `shouldStartWith` [1]

    it "shouldContail" $ do
      [1,2] `shouldContain` [1]
      

  describe "Intro" $ do
    it ":" $ do 
      'A':" SMALL CAT" `shouldBe` "A SMALL CAT"
