module Learn.Ch3RecursionSpec where

import Test.Hspec
import Data.Char 


-- Recursion
natSum :: (Num a, Ord a) => a -> a
natSum 0              = 0
natSum n  | n > 0     = n + natSum (n - 1) 
          | otherwise = error "natSum: Input value too small!"


-- List Construction
suffixes :: String -> [String]
suffixes ""  = []
suffixes str = str : suffixes (tail str)


-- Mapping
allSquares :: Num a => [a] -> [a]
allSquares []       = []
allSquares (x : xs) = x * x : allSquares xs


allToUpper :: String -> String
allToUpper []                 = []
allToUpper (chr : restString) = toUpper chr : allToUpper restString


-- Reduction
myproduct :: Num a => [a] -> a
myproduct []     = 1
myproduct (x:xs) = x * product xs

mysum :: Num a => [a] -> a
mysum []     = 0
mysum (x:xs) = x + sum xs



spec :: Spec
spec = do
  it "natSum" $ do
    natSum 5 `shouldBe` 15

  it "list Construction" $ do 
    suffixes "ello" `shouldBe` ["ello", "llo", "lo", "o"]

  it "Mapping" $ do
    allSquares [1,2,3] `shouldBe` [1,4,9]
    allToUpper "can you hear me now?" `shouldBe` "CAN YOU HEAR ME NOW?"

  it "Reduction" $ do
    myproduct [1,2,3] `shouldBe` 6
    mysum [1,2,3] `shouldBe` 6
    concat [[5, 6, 2], [], [4, 2]] `shouldBe` [5,6,2,4,2]
  
