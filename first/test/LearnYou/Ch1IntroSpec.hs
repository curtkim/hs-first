module LearnYou.Ch1IntroSpec where

import Test.Hspec

spec :: Spec
spec = do

  describe "Intro" $ do
    it ":" $ do 
      'A':" SMALL CAT" `shouldBe` "A SMALL CAT"
