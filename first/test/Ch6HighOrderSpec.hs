-- http://learnyouahaskell.com/higher-order-functions
module Ch6HighOrderSpec where

import Test.Hspec


-- Curried Function
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z 

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 

-- Some higher-orderism is in order
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 

flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y  

map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map f xs 


quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
  let smallerSorted = quicksort (filter (<=x) xs)  
      biggerSorted = quicksort (filter (>x) xs)   
  in  smallerSorted ++ [x] ++ biggerSorted  


-- Only folds and horses
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  



spec :: Spec
spec = do

  describe "Curried" $ do
    it "max" $ do 
      max 4 5 `shouldBe` 5
      (max 4) 5 `shouldBe` 5

    it "multiThree" $ do
      let multTwoWithFour = multThree 4
      multTwoWithFour 2 3  `shouldBe` 2*3*4

    it "infix by section" $ do
      divideByTen 200 `shouldBe` 20

  describe "Some higher-orderism is in order" $ do
    it "applyTwice" $ do
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
      flip' zip [1,2] "hi" `shouldBe` [('h',1),('i',2)]  
    
    it "map" $ do 
      map' (+3) [1,5] `shouldBe` [4,8]

    it "filter" $ do
      quicksort [1,5,3] `shouldBe` [1,3,5]
    
    it "takeWhile" $ do
      takeWhile (/=' ') "elephants know how to party" `shouldBe` "elephants"

    it "find the sum of all odd squares that are smaller than 10,000" $ do
      sum (takeWhile (<10) (filter odd (map (^2) [1..]))) `shouldBe` 1+9
      sum (takeWhile (<10) [n^2 | n <- [1..], odd (n^2)]) `shouldBe` 1+9

  describe "Only folds and horses" $ do
    it "sum'" $ do
      sum' [3,5,2,1] `shouldBe` 11

    it "scan" $ do
      scanl (+) 0 [3,5,2,1] `shouldBe` [0,3,8,10,11]
      scanr (+) 0 [3,5,2,1] `shouldBe` [11,8,3,1,0] 

      scanl1 (+) [3,5,2,1] `shouldBe` [3,8,10,11] 
      scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1] `shouldBe` [3,4,5,5,7,9,9,9]

  describe "Function application with $" $ do
    it "$" $ do
      sqrt (3 + 4 + 9) `shouldBe` (sqrt $ 3 + 4 + 9)
      sum (filter (> 10) (map (*2) [2..10])) `shouldBe` (sum $ filter (> 10) $ map (*2) [2..10])
      map ($ 3) [(4+), (10*), (^2)] `shouldBe` [7.0, 30.0, 9.0]

  it "Function composition" $ do
    map (negate . abs) [5,-3] `shouldBe` [-5, -3]