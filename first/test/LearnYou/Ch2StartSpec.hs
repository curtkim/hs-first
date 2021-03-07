module LearnYou.Ch2StartSpec where

import Test.Hspec




curt = "It's a-me, curt!"  -- 함수인가? 정의인가?

spec :: Spec
spec = do

  describe "start" $ do
    it "==" $ do
      5 == 5 `shouldBe` True

  describe "func" $ do
    it "doubleMe" $ do
      let doubleMe x = x+x
      doubleMe 1 `shouldBe` 2

    it "with_if" $ do
      let doubleSmallNumber x = if x > 100
                                then x
                                else x*2

      doubleSmallNumber 101 `shouldBe` 101
      doubleSmallNumber 2 `shouldBe` 4

    it "func or string definition" $ do 
      curt `shouldBe` "It's a-me, curt!"   

  describe "List" $ do

    it "elem" $ do
      4 `elem` [3,4,5] `shouldBe` True
      10 `elem` [3,4,5] `shouldBe` False

    it "++" $ do 
      [1,2,3,4] ++ [9,10,11,12] `shouldBe` [1,2,3,4,9,10,11,12] 

    it ":" $ do
      0:[1,2] `shouldBe` [0,1,2]
      [1,2,3] `shouldBe` 1 : 2 : 3 : []

    it "!!" $ do
      [0,1,2,3] !! 1 `shouldBe` 1
      [0.0, 1.0, 2.0, 3.0] !! 1 `shouldBe` 1.0
    
    it "compare" $ do
      [3,2,1] == [3,2,1] `shouldBe` True
      [3,2,1] > [2,4,5] `shouldBe` True
      
    it "head tail" $ do 
      head [5,4,3,2,1] `shouldBe` 5
      tail [5,4,3,2,1] `shouldBe` [4,3,2,1]

    it "head empty list Exception" $ do
      head [] `shouldThrow` anyException 
      head [] `shouldThrow` anyErrorCall

    it "init last" $ do
      init [5,4,3,2,1] `shouldBe` [5,4,3,2]
      last [5,4,3,2,1] `shouldBe` 1
    
    it "length" $ do
      length [1,2] `shouldBe` 2
    
    it "null" $ do
      null [1,2,3] `shouldBe` False 
      null [] `shouldBe` True

    it "maximum" $ do
      maximum [1,2,3] `shouldBe` 3
    
    it "product" $ do
      product [1,2,3] `shouldBe` 6

  describe "Range" $ do
    it "create" $ do
      [1..3] `shouldBe` [1,2,3]
      ['a'..'c'] `shouldBe` "abc"
      ['A'..'C'] `shouldBe` "ABC"

    it "step" $ do
      [2,4..10] `shouldBe` [2,4,6,8,10]
      [3,6..10] `shouldBe` [3,6,9]

  describe "infinite list" $ do
    it "cycle" $ do
      take 5 (cycle [1,2,3]) `shouldBe` [1,2,3,1,2]

    it "repeat" $ do
      take 5 (repeat 5) `shouldBe` [5,5,5,5,5]

  describe "list comprehension" $ do

    it "first" $ do
      [x*2 | x <- [1,2]] `shouldBe` [2,4]
      [x*2 | x <- [1..10], x >= 6] `shouldBe` [12,14,16,18,20]

    it "with if" $ do
      [if x == 1 then "foo" else "bar" | x <- [1,2]] `shouldBe` ["foo","bar"]

    it "multi predicate" $ do
      [x | x <- [10..20], x /=13, x/=15, x/=19] `shouldBe` [10,11,12,14,16,17,18,20]  

    it "multi list" $ do
      [x*y | x <- [2,5,10], y <- [8,10]] `shouldBe` [16,20, 40,50, 80,100]   

    it "length`" $ do
      let length' xs = sum [1 | _ <- xs]
      length' [1,2] `shouldBe` 2
    
    it "removeNonUpperCase" $ do
      let removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]
      removeNonUpperCase "Ha" `shouldBe` "H"

    it "nested" $ do
      let xxs = [[1,3],[1,2],[1,2,4]]
      [ [ x | x <- xs, even x ] | xs <- xxs] `shouldBe` [[],[2],[2,4]]

  describe "zip" $ do
    it "first" $ do
      zip [1,2,3] [5,5,5] `shouldBe` [(1,5),(2,5),(3,5)]
      zip [1,2,3] [5,5] `shouldBe` [(1,5),(2,5)]
      zip [1..] [5,5] `shouldBe` [(1,5),(2,5)]

