module Wiki2103FoldableSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Debug.Trace


plusplus x y = trace ("call plusplus x="++ show x ++ " y=" ++ show y) (x ++ y)


spec :: Spec
spec = do

  it "foldr" $ do       
    foldl (+) 0 [1,2,3] `shouldBe` 6

    -- call plusplus x="c" y=""
    -- call plusplus x="b" y="c"
    -- call plusplus x="a" y="bc"
    foldr plusplus "" ["a", "b", "c"] `shouldBe` "abc"
    
    -- call plusplus x="" y="a"
    -- call plusplus x="a" y="b"
    -- call plusplus x="ab" y="c"    
    foldl plusplus "" ["a", "b", "c"] `shouldBe` "abc"

    mconcat ["Tree", "fingers"] `shouldBe` "Treefingers"