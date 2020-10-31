module L5HighOrderSpec where

import Test.Hspec
import Data.Char 

allSquares :: Num a => [a] -> [a]
allSquares xs = map square xs
  where
    square x = x * x


allSquares2 :: Num a => [a] -> [a]
allSquares2 xs = map (\x -> x * x) xs


allToUpper :: String -> String
allToUpper string = map toUpper string


average :: Float -> Float -> Float
average a b = (a + b) / 2.0

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith op []       _        = []
myZipWith op _        []       = []
myZipWith op (x : xs) (y : ys) = (x `op` y) : myZipWith op xs ys


-- Reduction
mysum :: Num a => [a] -> a
mysum = foldr (+) 0 

allEven :: [Int] -> Bool
allEven []     = True
allEven (x:xs) = even x && allEven xs


-- Function composition
sumOfSquareRoots0 :: (Ord a, Floating a) => [a] -> a
sumOfSquareRoots0 xs = sum (allSquareRoots (filterPositives xs))
  where
    allSquareRoots []     = []
    allSquareRoots (x:xs) = sqrt x : allSquareRoots xs

    filterPositives [] 
      = []
    filterPositives (x:xs)
      | x > 0     = x : filterPositives xs
      | otherwise = filterPositives xs        


sumOfSquareRoots xs = sum $ map sqrt $ filter (> 0) xs

sumOfSquareRoots1 = sum . map sqrt . filter (> 0) 


spec :: Spec
spec = do

  it "Using map" $ do
    allSquares [1,2] `shouldBe` [1,4]
    allToUpper "abc" `shouldBe` "ABC"

  it "zipWith" $ do
    zipWith average [1, 2, 3] [4, 5, 6] `shouldBe` [2.5, 3.5, 4.5]
    myZipWith average [1, 2, 3] [4, 5, 6] `shouldBe` [2.5, 3.5, 4.5]

  it "Anonymous function" $ do
    allSquares2 [1,2] `shouldBe` [1,4]

  it "Reduction" $ do
    mysum [1,2] `shouldBe` 3
    allEven [1,2] `shouldBe` False

  it "Function application with lower precedence" $ do
    (sqrt $ average 60 30) `shouldBe` (sqrt (average 60 30))

  it "Function composition" $ do
    sumOfSquareRoots [1,4,9]  `shouldBe` 6
    sumOfSquareRoots0 [1,4,9]  `shouldBe` 6
    sumOfSquareRoots1 [1,4,9]  `shouldBe` 6
