module Wiki2109LensSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- 평면 상의 점.
data Point = Point
    { positionX :: Double
    , positionY :: Double
    } deriving (Show, Eq)

-- 한 점에서 다른 점으로 가는 선분.
data Segment = Segment
    { segmentStart :: Point
    , segmentEnd :: Point
    } deriving (Show, Eq)

-- 점과 선분을 만들기 위한 보조 함수들.
makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)


spec :: Spec
spec = do

  it "let in expression" $ do 
    (let x = 2 in x*2) `shouldBe` 4
    (let x = 2 in x*2) + 3 `shouldBe` 7

  describe "Without Lens" $ do
    let testSeg = makeSegment (0, 1) (2, 4)

    it "native get" $ do
      (positionY . segmentEnd $ testSeg) `shouldBe` 4.0

    it "native set" $ do      
      testSeg { segmentEnd = makePoint (2, 3) } `shouldBe` makeSegment (0, 1) (2, 3)
      let end = segmentEnd testSeg in testSeg { segmentEnd = end { positionY = 2 * positionY end }} `shouldBe` makeSegment (0, 1) (2, 8)