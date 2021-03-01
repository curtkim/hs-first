module LearnYou.Ch8MyTypeSpec where

import Test.Hspec


data Person0 = Person0 String String Int Float String String deriving (Show)  

data Person = Person { 
  firstName :: String, 
  lastName :: String, 
  age :: Int, 
  height :: Float, 
  phoneNumber :: String, 
  flavor :: String
} deriving (Show)

data Car = Car {
  company :: String, 
  model :: String, 
  year :: Int
} deriving (Show)  


-- Type Parameters(c++ template)
-- data Maybe a = Nothing | Just a 
-- 1) a : type paramter
-- 2) Maybe : type constructor


-- Type Class
data Person2 = Person2 {   
  fName :: String, 
  lName :: String, 
  myage :: Int  
} deriving (Eq, Show, Read) 


-- yes or no typeclass
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where  
  yesno 0 = False  
  yesno _ = True  

instance YesNo [a] where  
  yesno [] = False  
  yesno _ = True  

instance YesNo Bool where  
  yesno = id

instance YesNo (Maybe a) where  
  yesno (Just _) = True  
  yesno Nothing = False  

yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult  



spec :: Spec
spec = do

  it "record" $ do
    let guy = Person0 "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
    show guy `shouldBe` "Person0 \"Buddy\" \"Finklestein\" 43 184.2 \"526-2928\" \"Chocolate\""
  
  it "record car" $ do
    let car = Car {company="Ford", model="Mustang", year=1967}  
    show car `shouldBe` "Car {company = \"Ford\", model = \"Mustang\", year = 1967}"
    company car `shouldBe` "Ford"
    year car `shouldBe` 1967

  it "type class" $ do
    let adRock = Person2 {fName = "Adam", lName = "Horovitz", myage = 41}  
    let mca = Person2 {fName = "Adam", lName = "Yauch", myage = 44}  
    mca == adRock `shouldBe` False
    mca == mca `shouldBe` True
    
  it "yesno" $ do
    yesno "haha" `shouldBe` True
    yesno (length []) `shouldBe` False
    yesno (Just 0) `shouldBe` True

    yesnoIf [] "YEAH" "NO" `shouldBe` "NO"
