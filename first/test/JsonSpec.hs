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



spec :: Spec
spec = do

  describe "Json" $ do
    it "decode" $ do      
      maybeFoo `shouldBe` Just Foo {field1=27, field2="hello!"} 
      encode maybeFoo `shouldBe` "{\"field1\":27,\"field2\":\"hello!\"}"
