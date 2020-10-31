module StringSpec where

import Test.Hspec

import Data.Char  
import Data.List 


spec :: Spec
spec = do

  describe "String" $ do   
    it "doit" $ do  
      words "a b c" `shouldBe` ["a", "b", "c"]
      (reverse . words) "a b c" `shouldBe` ["c", "b", "a"]
      (intersperse "-" . reverse . words) "a b c" `shouldBe` ["c", "-", "b", "-", "a"]

      map toUpper "abc" `shouldBe` "ABC"
      (reverse . map toUpper) "abc" `shouldBe` "CBA"
      (intersperse '-' . reverse . map toUpper) "abc" `shouldBe` "C-B-A"
