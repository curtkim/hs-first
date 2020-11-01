{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Wiki2109LensSpec2 where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Control.Lens

data Point = Point
    { _positionX :: Double
    , _positionY :: Double
    } deriving (Show, Eq)
makeLenses ''Point

data Segment = Segment
    { _segmentStart :: Point
    , _segmentEnd :: Point
    } deriving (Show, Eq)
makeLenses ''Segment

makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)



spec :: Spec
spec = do

  describe "Lens" $ do    
    let testSeg = makeSegment (0, 1) (2, 4)

    it "get" $ do      
      view segmentEnd testSeg `shouldBe` makePoint (2,4)

    it "set" $ do      
      set segmentEnd (makePoint (2, 3)) testSeg `shouldBe` makeSegment (0, 1) (2, 3)

    it "nested get" $ do
      view (segmentEnd . positionY) testSeg `shouldBe` 4.0
      over (segmentEnd . positionY) (2 *) testSeg `shouldBe` makeSegment (0, 1) (2, 8)
