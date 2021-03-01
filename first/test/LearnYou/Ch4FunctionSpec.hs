module LearnYou.Ch4FunctionSpec where

import Test.Hspec

-- pattern matching
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1) 

first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z 


-- guard
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
  | bmi <= 18.5 = "You're underweight, you emo, you!"  
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
  | otherwise   = "You're a whale, congratulations!"  

myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
  | a > b     = GT  
  | a == b    = EQ  
  | otherwise = LT  


-- where
bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height  
  | bmi <= skinny = "You're underweight, you emo, you!"  
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
  | otherwise     = "You're a whale, congratulations!"  
  where 
    bmi = weight / height ^ 2  
    skinny = 18.5  
    normal = 25.0  
    fat = 30.0  


-- let
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
  let sideArea = 2 * pi * r * h  
      topArea = pi * r ^2  
  in  sideArea + 2 * topArea  


sum' :: (Num a) => [a] -> a     
sum' xs = foldl (+) 0 xs   

sum2' :: (Num a) => [a] -> a     
sum2' = foldl (+) 0

fn x = ceiling (negate (tan (cos (max 50 x))))  
fn2 = ceiling . negate . tan . cos . max 50


oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10) . filter odd . map (^2) $ [1..] 

oddSquareSum2 :: Integer  
oddSquareSum2 =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10) oddSquares  
    in  sum belowLimit 


-- case
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  


head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  


spec :: Spec
spec = do

  describe "Pattern Matching" $ do 
    it "sayme" $ do
      sayMe 1 `shouldBe` "One!"
    it "factorial" $ do
      factorial 2 `shouldBe` 2
    it "tripple" $ do
      first (1,2,3) `shouldBe` 1
      second (1,2,3) `shouldBe` 2
      third (1,2,3) `shouldBe` 3
    it "pattern matching in list comprehension" $ do
      let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
      [a+b | (a,b) <- xs] `shouldBe` [4,7,6,8,11,4] 

  describe "Guard" $ do
    it "bmiTell" $ do
      bmiTell 10 `shouldBe` "You're underweight, you emo, you!"
    it "myCompare" $ do
      (3 `myCompare` 2) `shouldBe` GT      

  describe "Where" $ do
    it "bmiTell2" $ do
      bmiTell2 60 180 `shouldBe` "You're underweight, you emo, you!"


  describe "let" $ do
    it "cylinder" $ do
      cylinder 1 1 `shouldBe` 12.566370614359172

    it "let" $ do
      4 * (let a = 9 in a + 1) + 2 `shouldBe` 42
      [let square x = x * x in (square 5, square 3, square 2)] `shouldBe` [(25,9,4)]

      (let (a,b,c) = (1,2,3) in a+b+c) `shouldBe` 6

  describe "case" $ do 
    it "describeList" $ do
      describeList [1] `shouldBe` "The list is a singleton list."

    it "head'" $ do 
      head' [1,2] `shouldBe` 1



  it "function composition(dot)" $ do 
    map (negate . abs) [5,-3,-6] `shouldBe` [-5,-3,-6]

  it "point free style" $ do 
    sum' [1,2,3] `shouldBe` 6
    sum2' [1,2,3] `shouldBe` 6

  it "math pow" $ do 
    map (^2) [1,2] `shouldBe` [1,4]

  it "oddSquareSum" $ do
    oddSquareSum `shouldBe` 1^2 + 3^2 