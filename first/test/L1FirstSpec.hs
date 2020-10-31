module L1FirstSpec where

import Test.Hspec

-- Type Signature
inc :: Int -> Int     -- type signature
inc x = x + 1         -- function equation

-- Multiple Arguments
average :: Float -> Float -> Float
average a b  = (a + b) / 2.0


spec :: Spec
spec = do
  it "Type Signature" $ do
    inc 1 `shouldBe` (2 :: Int)

  it "Multiple Arguments" $ do 
    average 3.0 4.0 `shouldBe` 3.5

  it "infix" $ do 
    (3.0 `average` 4.0) `shouldBe` 3.5
    (+) 1 5 `shouldBe` 6
