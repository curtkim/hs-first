module Learn.Ch2FundamentalSpec where

import Test.Hspec

import Simple -- from src


-- Branch
mymax :: Ord a => a -> a -> a
mymax x y | x >= y     = x 
          | otherwise  = y

-- Associating Names with Values or Functions
circleArea :: Floating a => a -> a
circleArea radius = pi * radius * radius

circleArea' :: Floating a => a -> a
circleArea' diameter = pi * radius * radius
  where
    radius = diameter / 2.0       -- local binding


-- Tuple
type Point = (Int, Int)

-- origin of the coordinate system
--
origin :: Point
origin  = (0, 0)

-- move a given point to the right
--
moveRight :: Point -> Int -> Point
moveRight (x, y) distance  = (x + distance, y)


-- List
-- return all positive odd numbers up to maxNumber
oddNumbers :: Int -> [Int]
oddNumbers maxNumber  = [1, 3..maxNumber]


spec :: Spec
spec = do
  it "Modules" $ do
    arithmetic_mean 1 2 `shouldBe` 1.5

  it "Branch" $ do 
    mymax 1 2 `shouldBe` 2

  it "Associating Names with Values or Functions" $ do
    circleArea' 6.0 `shouldSatisfy` (> 28.2743)
    circleArea' 6.0 `shouldSatisfy` (< 28.2744)

  it "Tuple" $ do 
    fst origin `shouldBe` 0
    moveRight origin 1 `shouldBe` (1, 0)

  describe "List" $ do

    it "List" $ do
      oddNumbers 10 `shouldBe` [1, 3, 5, 7, 9]

    it "Add to front" $ do
      "blue" : [] `shouldBe` ["blue"]
      "yellow" : ["red", "green", "blue"] `shouldBe` ["yellow", "red", "green", "blue"]
    
    it "join" $ do 
      [4, 2, 3] ++ [3, 1, 2, 7] `shouldBe` [4, 2, 3, 3, 1, 2, 7]

    it "extract at specific index" $ do 
      [0, 1, 2, 3] !! 2  `shouldBe` 2
      "Hello" !! 4 `shouldBe` 'o'

    it "head tail" $ do
      head [0, 1, 2, 3] `shouldBe` 0
      tail [0, 1, 2, 3]  `shouldBe` [1, 2, 3]
      head  "mouse" `shouldBe` 'm'
      tail  "mouse" `shouldBe` "ouse"

    it "head empty" $ do 
      head [] `shouldThrow` anyException

    it "length" $ do
      length [0, 1, 2, 3] `shouldBe` 4

    it "element exist check" $ do
      elem 2 [0, 1, 2, 3] `shouldBe` True

    it "sum" $ do 
      sum [0, 1, 2, 3] `shouldBe` 6
