{-# LANGUAGE TemplateHaskell #-}
module LensSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.Text
import Control.Lens

data User = User
  { _name     :: String
  , _userid   :: Int
  , _metadata :: UserInfo
  }
  deriving (Show)

data UserInfo = UserInfo
  { _numLogins     :: Int
  , _associatedIPs :: [String]
  }
  deriving (Show)

makeLenses ''User
makeLenses ''UserInfo


user1 = User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }


spec :: Spec
spec = do

  -- https://hackage.haskell.org/package/lens
  describe "Lens" $ do
    it "basic ^." $ do 
      view _2 ("hello","world") `shouldBe` "world"
      ("hello","world")^._2 `shouldBe` "world"
      ("hello","world") ^._2 `shouldBe` "world"

    it "nest" $ do
      ("hello",("world","!!!"))^._2._1 `shouldBe` "world"
      set (_2._1) 42 ("hello",("world","!!!")) `shouldBe` ("hello",(42,"!!!"))
    
    it "set .~" $ do
      set _2 42 ("hello","world") `shouldBe` ("hello",42)
      (_1 .~ "hello" $ ((),"world")) `shouldBe` ("hello","world")

    it "over %~" $ do
      over mapped succ [1,2,3] `shouldBe` [2,3,4]
      over (mapped._2) succ [(1,2),(3,4)] `shouldBe` [(1,3),(3,5)]
      (_1.mapped._2.mapped %~ succ $ ([(42, "hello")],"world")) `shouldBe` ([(42, "ifmmp")],"world")

    it "*~" $ do 
      (both *~ 2 $ (1,2)) `shouldBe` (2,4)


    describe "User" $ do
      it "get" $ do
        user1 ^. name `shouldBe` "qiao.yifan"
        user1 ^. metadata.numLogins `shouldBe` 20        

      it "set" $ do
        show (user1 & metadata.numLogins .~ 0) `shouldBe` "User {_name = \"qiao.yifan\", _userid = 103, _metadata = UserInfo {_numLogins = 0, _associatedIPs = [\"52.39.193.61\",\"52.39.193.75\"]}}"
        show (user1 & metadata.associatedIPs %~ ("192.168.0.2" :)) `shouldBe` "User {_name = \"qiao.yifan\", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = [\"192.168.0.2\",\"52.39.193.61\",\"52.39.193.75\"]}}"
        show (metadata.numLogins %~ (+ 1) $ user1) `shouldBe` "User {_name = \"qiao.yifan\", _userid = 103, _metadata = UserInfo {_numLogins = 21, _associatedIPs = [\"52.39.193.61\",\"52.39.193.75\"]}}"

        show (user1 & metadata .~ (UserInfo 17 [])) `shouldBe` "User {_name = \"qiao.yifan\", _userid = 103, _metadata = UserInfo {_numLogins = 17, _associatedIPs = []}}"
        show (metadata.associatedIPs .~ [ "50.193.0.23" ] $ user1) `shouldBe` "User {_name = \"qiao.yifan\", _userid = 103, _metadata = UserInfo {_numLogins = 20, _associatedIPs = [\"50.193.0.23\"]}}"

      it "exam" $ do
        -- Get the associated IP addresses.
        user1 ^. metadata.associatedIPs `shouldBe` [ "52.39.193.61", "52.39.193.75"]        
        -- Update the user so that the associated IP addresses are in reverse order.
        -- show (user1 & metadata.associatedIPs %~ reverse?? )
        -- Update the user so that each word in the name is capitalized.
        -- Set the number of logins to 1.
        -- Remove all associated IP addresses except the first.