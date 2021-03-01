module Learn.Ch6DataTypeSpec where

import Test.Hspec


type Point = (Float, Float)
type Line  = (Point, Point)

data LineStyle
  = Solid
  | Dashed
  | Dotted

type FancyLine = (Point, Point, LineStyle)


data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Enum)

isWeekday :: Day -> Bool
isWeekday Sunday   = False
isWeekday Saturday = False
isWeekday _        = True


isWeekday2 :: Day -> Bool
isWeekday2 day = case day of
                  Sunday   -> False
                  Saturday -> False
                  _        -> True

isWeekday3 :: Day -> Bool
isWeekday3 day = case day of {Sunday -> False ; Saturday -> False; _ -> True}

isWeekday4 :: Day -> Bool
isWeekday4 day = not $ day `elem` [Saturday, Sunday]


spec :: Spec
spec = do

  it "Pattern matching" $ do
    isWeekday Monday `shouldBe` True
    isWeekday2 Monday `shouldBe` True
    isWeekday3 Monday `shouldBe` True
    isWeekday4 Monday `shouldBe` True


