-- http://learnyouahaskell.com/higher-order-functions
module LearnYou.Ch6HighOrderSpec where

import Test.Hspec



spec :: Spec
spec = do

  describe "Curried" $ do
    it "max" $ do 
      max 4 5 `shouldBe` 5
      (max 4) 5 `shouldBe` 5

    it "multiThree" $ do
      let multThree :: (Num a) => a -> a -> a -> a  
          multThree x y z = x * y * z 
      let multTwoWithFour = multThree 4
      multTwoWithFour 2 3  `shouldBe` 4*2*3

    it "infix by section" $ do
      let divideByTen :: (Floating a) => a -> a  
          divideByTen = (/10) 
      divideByTen 200 `shouldBe` 20

  describe "Some higher-orderism is in order" $ do
    it "applyTwice" $ do
      let applyTwice :: (a -> a) -> a -> a  
          applyTwice f x = f (f x) 

      applyTwice (+3) 10 `shouldBe` 16
      applyTwice (3:) [1] `shouldBe` [3,3,1]  
      applyTwice (++ " HAHA") "HEY" `shouldBe` "HEY HAHA HAHA"      
      applyTwice ("HAHA " ++) "HEY" `shouldBe` "HAHA HAHA HEY"

    it "zipWith" $ do
      zipWith (+) [1, 2, 3] [4, 5, 6] `shouldBe` [5,7,9]
      (zipWith (+) [4,2,5,6] [2,6,2,3]) `shouldBe` [6,8,7,9] 
      (replicate 5 2) `shouldBe` [2,2,2,2,2]
      (zipWith (*) (replicate 5 2) [1..]) `shouldBe` [2,4,6,8,10]

    it "flip" $ do
      zip [1,2] "hi" `shouldBe` [(1,'h'),(2,'i')]

      let flip' :: (a -> b -> c) -> b -> a -> c  
          flip' f y x = f x y  

      flip' zip [1,2] "hi" `shouldBe` [('h',1),('i',2)]  
    
    it "map fun list" $ do 
      let map' :: (a -> b) -> [a] -> [b]  
          map' _ [] = []  
          map' f (x:xs) = f x : map f xs 

      map' (+3) [1,5] `shouldBe` [4,8]

    it "filter" $ do
      filter even [1..10] `shouldBe` [2,4,6,8,10]

      let quicksort :: (Ord a) => [a] -> [a]    
          quicksort [] = []    
          quicksort (x:xs) =     
            let smallerSorted = quicksort (filter (<=x) xs)  
                biggerSorted = quicksort (filter (>x) xs)   
            in  smallerSorted ++ [x] ++ biggerSorted  

      quicksort [1,5,3] `shouldBe` [1,3,5]
    
    it "takeWhile" $ do
      takeWhile (/=' ') "elephants know how to party" `shouldBe` "elephants"

    it "find the sum of all odd squares that are smaller than 10,000" $ do
      sum (takeWhile (<10) (filter odd (map (^2) [1..]))) `shouldBe` 1+9
      sum (takeWhile (<10) [n^2 | n <- [1..], odd (n^2)]) `shouldBe` 1+9

  describe "lambda" $ do
    it "first" $ do
      map (\(a,b) -> a + b) [(1,2),(3,5)] `shouldBe` [3,8]  


  describe "Only folds and horses" $ do
    it "left fold" $ do
      let sum' :: (Num a) => [a] -> a  
          sum' xs = foldl (\acc x -> acc + x) 0 xs  

      sum' [3,5,2,1] `shouldBe` 11

    it "right fold" $ do
      let map' :: (a -> b) -> [a] -> [b]
          map' f xs = foldr (\x acc -> f x : acc) [] xs 
      map' (+1) [1,2] `shouldBe` [2,3]

    it "foldl1 foldr1" $ do
      foldl1 (\acc x -> acc + x) [1,2,3] `shouldBe` 6
      foldr1 (\acc x -> acc + x) [1,2,3] `shouldBe` 6

    it "scan" $ do
      scanl (+) 0 [3,5,2,1] `shouldBe` [0,3,8,10,11]
      scanr (+) 0 [3,5,2,1] `shouldBe` [11,8,3,1,0] 

      scanl1 (+) [3,5,2,1] `shouldBe` [3,8,10,11] 
      scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1] `shouldBe` [3,4,5,5,7,9,9,9]

  describe "Function application with $(lowest precedence)" $ do

    -- ($) :: (a -> b) -> a -> b  
    -- f $ x = f x     

    it "$" $ do
      sqrt (3 + 4 + 9) `shouldBe` (sqrt $ 3 + 4 + 9)
      sum (filter (> 10) (map (*2) [2..10])) `shouldBe` (sum $ filter (> 10) $ map (*2) [2..10])

      map ($ 3) [(4+), (10*), (^2)] `shouldBe` [7.0, 30.0, 9.0]

  describe "Function composition" $ do

    it "dot" $ do 
      map (negate . abs) [5,-3,-6] `shouldBe` [-5,-3,-6]

    it "point free style" $ do 
      let sum' :: (Num a) => [a] -> a     
          sum' xs = foldl (+) 0 xs   

      let sum2' :: (Num a) => [a] -> a     
          sum2' = foldl (+) 0

      sum' [1,2,3] `shouldBe` 6
      sum2' [1,2,3] `shouldBe` 6

    it "math pow" $ do 
      map (^2) [1,2] `shouldBe` [1,4]

    it "oddSquareSum" $ do

      let oddSquareSum :: Integer  
          oddSquareSum = sum . takeWhile (<10) . filter odd . map (^2) $ [1..] 

      let oddSquareSum2 :: Integer  
          oddSquareSum2 =   
              let oddSquares = filter odd $ map (^2) [1..]  
                  belowLimit = takeWhile (<10) oddSquares  
              in  sum belowLimit 

      oddSquareSum `shouldBe` 1^2 + 3^2 
