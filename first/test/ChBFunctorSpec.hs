module ChBFunctorSpec where

import Test.Hspec

import Data.Char  
import Data.List 
import Control.Applicative

{-|

class (Functor f) => Applicative f where  
  pure :: a -> f a  
  (<*>) :: f (a -> b) -> f a -> f b 

instance Applicative Maybe where  
  pure = Just  
  Nothing <*> _ = Nothing  
  (Just f) <*> something = fmap f something  

instance Applicative [] where  
  pure x = [x]  
  fs <*> xs = [f x | f <- fs, x <- xs] 

instance Applicative IO where  
  pure = return  
  a <*> b = do  
    f <- a  
    x <- b  
    return (f x)  
-}

spec :: Spec
spec = do

  describe "Functor" $ do   
    it "fmap" $ do  
      fmap (intersperse '-' . reverse . map toUpper) (Just "abc") `shouldBe` Just "C-B-A"
      fmap (intersperse '-' . reverse . map toUpper) ["abc", "de"] `shouldBe` ["C-B-A", "E-D"]

    it "fmap2" $ do
      fmap (replicate 3) [1,2,3] `shouldBe` [[1,1,1],[2,2,2],[3,3,3]]  
      fmap (replicate 3) (Just 4) `shouldBe` Just [4,4,4]

    it "law1 identity" $ do
      fmap id (Just 3) `shouldBe` (Just 3)
      fmap id [1..5] `shouldBe` [1,2,3,4,5]

  describe "Applicative" $ do
    it "do" $ do
      let a = fmap (*) [1,2,3,4]  
      fmap (\f -> f 9) a  `shouldBe` [9,18,27,36] 

    it "Maybe" $ do
      Just (+3) <*> Just 9 `shouldBe` Just 12
      pure (+3) <*> Just 9 `shouldBe` Just 12

      pure (+) <*> Just 3 <*> Just 5 `shouldBe` Just 8
      pure (+) <*> Just 3 <*> Nothing `shouldBe` Nothing
      pure (+) <*> Nothing <*> Just 5 `shouldBe` Nothing

    it "<$>" $ do
      (+) <$> Just 3 <*> Just 5 `shouldBe` Just 8
      (++) <$> Just "johntra" <*> Just "volta" `shouldBe` Just "johntravolta"

    it "List pure" $ do 
      (pure "Hey" :: [String]) `shouldBe` ["Hey"]
      (pure "Hey" :: Maybe String) `shouldBe` Just "Hey"

    it "List <*>" $ do
      [(*0),(+100),(^2)] <*> [1,2,3] `shouldBe` [0,0,0,101,102,103,1,4,9] 
      [(+),(*)] <*> [1,2] <*> [3,4] `shouldBe` [4,5,5,6,3,4,6,8]
      [(1+),(2+),(1*),(2*)] <*> [3,4] `shouldBe` [4,5,5,6,3,4,6,8]

      (++) <$> ["ha","heh","hmm"] <*> ["?","!","."] `shouldBe` ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

      [ x*y | x <- [2,5,10], y <- [8,10,11]] `shouldBe` [16,20,22,40,50,55,80,100,110]
      (*) <$> [2,5,10] <*> [8,10,11] `shouldBe` [16,20,22,40,50,55,80,100,110]

      (filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]) `shouldBe` [55,80,100,110] 

    it "ZipList" $ do
      (getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]) `shouldBe` [101,102,103]
      zipWith (+) [1,2,3] [3,2,1] `shouldBe` [4,4,4]

    it "liftA2" $ do 
      liftA2 (:) (Just 3) (Just [4]) `shouldBe` Just [3,4]
      (:) <$> Just 3 <*> Just [4] `shouldBe` Just [3,4] 

    it "sequenceA" $ do
      sequenceA [Just 3, Just 2, Just 1] `shouldBe` Just [3,2,1]  
      sequenceA [Just 3, Nothing, Just 1] `shouldBe` Nothing
      sequenceA [(+3),(+2),(+1)] 3 `shouldBe` [6,5,4]  
      sequenceA [[1,2,3],[4,5,6]] `shouldBe` [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

      map (\f -> f 7) [(>4),(<10),odd] `shouldBe` [True,True,True]
      (and $ map (\f -> f 7) [(>4),(<10),odd]) `shouldBe` True  
      (and $ sequenceA [(>4),(<10),odd] 7) `shouldBe` True
