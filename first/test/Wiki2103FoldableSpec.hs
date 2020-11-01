module Wiki2103FoldableSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)



spec :: Spec
spec = do

  it "foldr" $ do       
    foldl (+) 0 [1,2,3] `shouldBe` 6
    foldr (++) "" ["a", "b", "c"] `shouldBe` "abc"

    mconcat ["Tree", "fingers"] `shouldBe` "Treefingers"