data Point = Point
    { positionX :: Double
    , positionY :: Double
    } deriving (Show)

-- 한 점에서 다른 점으로 가는 선분.
data Segment = Segment
    { segmentStart :: Point
    , segmentEnd :: Point
    } deriving (Show)

-- 점과 선분을 만들기 위한 보조 함수들.
makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)