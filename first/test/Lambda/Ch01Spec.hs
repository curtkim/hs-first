module Lambda.Ch01Spec where

import Test.Hspec

f [] = []
f (x:xs) = f ys ++ [x] ++ zs
           where
             ys = [a | a <- xs, a <= x]
             zs = [b | b <- xs, b > x]

double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
avg ns = sum ns `div` length ns

-- by tuple
add :: (Int, Int) -> Int
add (x, y) = x + y

-- binary operator
add' :: Int -> Int -> Int
add' x y = x + y


--zeroto :: Int -> [Int]
--zeroto n :: [0..n]




spec :: Spec
spec = do

  --it "quicksort" $ do 
  --  f [3, 2, 9, 5, 4] `shouldBe` [2, 3, 4, 5, 9]

  it "avg" $ do
    avg [1,2,3] `shouldBe` 2

  describe "boolean" $ do
    it "basic" $ do
      False || True `shouldBe` True
    
    it "not" $ do
      not False `shouldBe` True


  describe "tuple" $ do
    it "fst snd" $ do
      fst (1, "Hello") `shouldBe` 1
      snd (1, "Hello") `shouldBe` "Hello"
      fst (snd (1, (2, 3))) `shouldBe` 2      

  describe "function" $ do
    it "add" $ do
      add (5, 5) `shouldBe` 10

    it "add'" $ do
      add' 5 5 `shouldBe` 10

    