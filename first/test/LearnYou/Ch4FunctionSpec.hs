module LearnYou.Ch4FunctionSpec where

import Test.Hspec

--fn x = ceiling (negate (tan (cos (max 50 x))))  
--fn2 = ceiling . negate . tan . cos . max 50

spec :: Spec
spec = do

  describe "Pattern Matching" $ do 
    it "sayme" $ do
      let sayMe :: (Integral a) => a -> String  
          sayMe 1 = "One!"  
          sayMe 2 = "Two!"  
          sayMe 3 = "Three!"  
          sayMe 4 = "Four!"  
          sayMe 5 = "Five!"  
          sayMe x = "Not between 1 and 5"  

      sayMe 1 `shouldBe` "One!"

    it "factorial" $ do
      let factorial :: (Integral a) => a -> a  
          factorial 0 = 1  
          factorial n = n * factorial (n - 1) 

      factorial 2 `shouldBe` 2

    it "tripple" $ do
      let first :: (a, b, c) -> a  
          first (x, _, _) = x  
      let second :: (a, b, c) -> b  
          second (_, y, _) = y          
      let third :: (a, b, c) -> c  
          third (_, _, z) = z 

      first (1,2,3) `shouldBe` 1
      second (1,2,3) `shouldBe` 2
      third (1,2,3) `shouldBe` 3

    it "tuple" $ do
      let addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)
      addVectors (1,2) (3,4) `shouldBe` (4,6)

    it "pattern matching in list comprehension" $ do
      let xs = [(1,3), (4,3)]  
      [a+b | (a,b) <- xs] `shouldBe` [4,7] 

    it "List" $ do 
      let head' :: [a] -> a
          head' [] = error "can't call head on an empty list"
          head' (x:_) = x

      head' [1,2,3] `shouldBe` 1

    it "List sum" $ do
      let sum' :: (Num a) => [a] -> a
          sum' [] = 0
          sum' (x:xs) = x + sum' xs
      
      sum' [1,2,3] `shouldBe` 6


  describe "Guard | " $ do
    it "bmiTell" $ do
      let bmiTell :: (RealFloat a) => a -> String  
          bmiTell bmi  
            | bmi <= 18.5 = "You're underweight, you emo, you!"  
            | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
            | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
            | otherwise   = "You're a whale, congratulations!"  

      bmiTell 10 `shouldBe` "You're underweight, you emo, you!"

    it "myCompare" $ do
      let myCompare :: (Ord a) => a -> a -> Ordering  
          a `myCompare` b  
            | a > b     = GT  
            | a == b    = EQ  
            | otherwise = LT  

      (3 `myCompare` 2) `shouldBe` GT      


  describe "Where" $ do
    it "bmiTell2" $ do
      let bmiTell2 :: (RealFloat a) => a -> a -> String  
          bmiTell2 weight height  
            | bmi <= skinny = "You're underweight, you emo, you!"  
            | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
            | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
            | otherwise     = "You're a whale, congratulations!"  
            where 
              bmi = weight / height ^ 2 -- 한번 계산된다.
              (skinny, normal, fat) = (18.5, 25.0, 30.0)

      bmiTell2 60 180 `shouldBe` "You're underweight, you emo, you!"


  describe "let <bindings> in <expression>" $ do
    it "cylinder" $ do
      let cylinder :: (RealFloat a) => a -> a -> a  
          cylinder r h = 
            let sideArea = 2 * pi * r * h  
                topArea = pi * r ^2  
            in  sideArea + 2 * topArea  

      cylinder 1 1 `shouldBe` 12.566370614359172

    it "let in" $ do
      4 * (let a = 9 in a + 1) + 2 `shouldBe` 42
      [let square x = x * x in (square 5, square 3, square 2)] `shouldBe` [(25,9,4)]
      (let (a,b,c) = (1,2,3) in a+b+c) `shouldBe` 6

    it "let in multiple bindings" $ do
      (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar) `shouldBe` (6000000,"Hey there!")


  describe "case" $ do 
    it "describeList" $ do
      let describeList :: [a] -> String  
          describeList xs = "The list is " ++ what xs  
              where what [] = "empty."  
                    what [x] = "a singleton list."  
                    what xs = "a longer list."  

      describeList [1] `shouldBe` "The list is a singleton list."

    it "head'" $ do 
      let head' :: [a] -> a  
          head' xs = case xs of [] -> error "No head for empty lists!"  
                                (x:_) -> x  

      head' [1,2] `shouldBe` 1


