-- http://learnyouahaskell.com/types-and-typeclasses
module Ch3TypeSpec where

import Test.Hspec

removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]  

removeNonUppercase2 :: String -> String
removeNonUppercase2 st = [ c | c <- st, c `elem` ['A'..'Z']]  

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  


spec :: Spec
spec = do

  it "removeNonUppercase" $ do
    removeNonUppercase "aBc" `shouldBe` "B"

  describe "TypeClass" $ do
    it "Show" $ do 
      show 3 `shouldBe` "3"

    it "Read" $ do 
      read "[1,2,3,4]" ++ [3] `shouldBe` [1,2,3,4,3]
      (read "5" :: Float) `shouldBe` 5.0
