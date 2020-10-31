module Main where

import Reanimate
import Reanimate.Builtin.Documentation
import Graphics.SvgTree


box :: SVG
box =
  withStrokeWidth 0
    (withFillOpacity 1
      (withFillColor "Black"
        (mkRect 1 1)))  -- (width, height)

queenSvg :: IO SVG
queenSvg = do
  Just queenDoc <- loadSvgFile "queen.svg"
  pure (center (scaleToWidth 1 (mkGroup (_elements queenDoc))))


main :: IO ()
main = reanimate (docEnv (drawBox `parA` drawCircle))
-- main = reanimate (mkAnimation 5 (\t -> box))
-- main = reanimate (mkAnimation 5 (\t -> rotate (360*t) box))
-- main = reanimate (mkAnimation 5 (\t -> translate (5*t) (3*t) box))
-- main = do
--   queen <- queenSvg
--   reanimate (mkAnimation 5 (\t -> queen))
