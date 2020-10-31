module ChCMonadSpec where -- Ch12MonadSpec

import Test.Hspec


-- walk the line
type Birds = Int  
type Pole = (Birds,Birds)  


landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  

landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing 

banana :: Pole -> Maybe Pole  
banana _ = Nothing 


foo :: Maybe String  
foo = do  
  x <- Just 3  
  y <- Just "!"  
  Just (show x ++ y)  

routine :: Maybe Pole  
routine = do  
  start <- return (0,0)  
  first <- landLeft 2 start  
  second <- landRight 2 first  
  landLeft 1 second

routine_banana :: Maybe Pole  
routine_banana = do  
  start <- return (0,0)  
  first <- landLeft 2 start  
  Nothing  
  second <- landRight 2 first  
  landLeft 1 second 


-- list
-- instance Monad [] where  
--     return x = [x]  
--     xs >>= f = concat (map f xs)  
--     fail _ = [] 



spec :: Spec
spec = do

  describe "walk the line" $ do   
    it "start" $ do  
      landLeft 2 (0,0) `shouldBe` Just (2,0)
      landLeft 10 (0,3) `shouldBe` Nothing
      
      (landRight 1 (0,0) >>= landLeft 2) `shouldBe` Just (2,1)  
      (Nothing >>= landLeft 2) `shouldBe` Nothing

      (return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) `shouldBe` Just (2,4) 
      (return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)) `shouldBe` Nothing
      (return (0,0) >>= landLeft 1 >>= banana >>= landRight 1) `shouldBe` Nothing


  describe "do notation" $ do
    it "start" $ do
      (Just 3 >>= (\x -> Just (show x ++ "!"))) `shouldBe` Just "3!"  
      foo `shouldBe` Just "3!"
    
    it "by pole" $ do 
      routine `shouldBe` Just (3,2)
      routine_banana `shouldBe` Nothing


  describe "the list monad" $ do 
    it "start" $ do
      ([3,4,5] >>= \x -> [x,-x]) `shouldBe` [3,-3,4,-4,5,-5] 
      map (\x -> [x,-x]) [3,4,5] `shouldBe` [[3,-3],[4,-4],[5,-5]]  
