-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Data.ByteString.Lazy as LB

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics


data Foo = Foo
  { field1 :: Int
  , field2 :: String
  }
  deriving (Show, Generic, Eq, ToJSON, FromJSON)

jsonString :: LB.ByteString
jsonString = "{ \"field1\": 27, \"field2\": \"hello!\" }"

maybeFoo :: Maybe Foo
maybeFoo = decode jsonString


--
data Person = Person
  { firstName :: String
  , lastName  :: String
  }
  deriving (Show, Eq)

-- our fields are snake_case instead
instance ToJSON Person where
  toJSON (Person { firstName = firstName, lastName = lastName }) =
    object [ "first_name" .= firstName
           , "last_name"  .= lastName
           ]

-- our fields are snake_case instead
instance FromJSON Person where
  -- note that the typeclass function is parseJSON, not fromJSON
  parseJSON = withObject "Person" $ \obj -> do
    firstName <- obj .: "first_name"
    lastName <- obj .: "last_name"
    return (Person { firstName = firstName, lastName = lastName })

karlJSON :: LB.ByteString
karlJSON = "{\"first_name\":\"Karl\",\"last_name\":\"Popper\"}"    


-- optional field
data Item = Item
  { name :: String
  , description :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \obj -> do
    name <- obj .: "name"
    description <- obj .:? "description"
    return (Item { name = name, description = description })

-- withObject :: String -> (Object -> Parser a) -> Value -> a
-- (.:)       :: FromJSON a => Object -> Text -> Parser a
-- (.:?)      :: FromJSON a => Object -> Text -> Parser (Maybe a)


-- enum data type
data UserType = User | Admin | CustomerSupport
  deriving (Generic, ToJSON) -- , FromJSON

instance FromJSON UserType where
  parseJSON = withText "UserType" $ \text ->
    case text of
      "user"             -> return User
      "admin"            -> return Admin
      "customer_support" -> return CustomerSupport
      _                  -> fail "string is not one of known enum values"


spec :: Spec
spec = do

  describe "Json" $ do
    it "decode" $ do      
      maybeFoo `shouldBe` Just Foo {field1=27, field2="hello!"} 
      maybeFoo `shouldBe` Just (Foo 27 "hello!")
      encode maybeFoo `shouldBe` "{\"field1\":27,\"field2\":\"hello!\"}"

  describe "custom field name" $ do
    it "toJson" $ do
      encode (Person "Karl" "Popper") `shouldBe` "{\"first_name\":\"Karl\",\"last_name\":\"Popper\"}"

    it "SfromJson" $ do
      (decode karlJSON :: Maybe Person) `shouldBe` Just (Person "Karl" "Popper")

  describe "optional field" $ do
    it "decode" $ do
      (decode "{\"name\": \"Very Evil Artifact\"}" :: Maybe Item) `shouldBe` Just (Item "Very Evil Artifact" Nothing)

  describe "enum" $ do
    it "encode" $ do
      encode CustomerSupport `shouldBe` "\"CustomerSupport\""
    