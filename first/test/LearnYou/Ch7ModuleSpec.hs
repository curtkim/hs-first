-- http://learnyouahaskell.com/modules
module LearnYou.Ch7ModuleSpec where

import Test.Hspec

import qualified Data.Map as Map  
import qualified Data.Set as Set  


-- List
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key [] = Nothing  
findKey key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey key xs 

findKey2 :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey2 key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing 


spec :: Spec
spec = do

  it "List.findKey" $ do
    let phoneBook = [
                      ("betty","555-2938"),  
                      ("bonnie","452-2928"),  
                      ("patsy","493-2928"),
                      ("lucille","205-2928"),  
                      ("wendy","939-8282"),  
                      ("penny","853-2492")
                    ]

    findKey "patsy" phoneBook `shouldBe` Just "493-2928"
    findKey "curt" phoneBook `shouldBe` Nothing

    findKey2 "patsy" phoneBook `shouldBe` Just "493-2928"
    findKey2 "curt" phoneBook `shouldBe` Nothing


  describe "Map" $ do 

    it "null" $ do 
      Map.null Map.empty `shouldBe` True
      Map.null (Map.fromList [(3,100)]) `shouldBe` False

    it "insert" $ do 
      Map.insert 3 100 Map.empty `shouldBe` Map.fromList [(3,100)]

    it "size" $ do
      Map.size (Map.fromList [(3,100)]) `shouldBe` 1

    it "member" $ do
      (Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]) `shouldBe` True

    it "lookup" $ do
      (Map.lookup 3 $ Map.fromList [(3,6),(4,3),(6,9)]) `shouldBe` Just 6

    it "singleton" $ do
      Map.singleton 3 9 `shouldBe` Map.fromList [(3,9)]


  describe "Set" $ do
    let text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
        text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
        set1 = Set.fromList text1  
        set2 = Set.fromList text2

    it "fromList" $ do
      set1 `shouldBe` Set.fromList " .?AIRadefhijlmnorstuy"  
      set2 `shouldBe` Set.fromList " !Tabcdefghilmnorstuvwy"
    
    it "intersection" $ do
      Set.intersection set1 set2 `shouldBe` Set.fromList " adefhilmnorstuy" 

    it "diff" $ do
      Set.difference set1 set2 `shouldBe` Set.fromList ".?AIRj"
