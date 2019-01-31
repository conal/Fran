-- A first curve editor.

module Editor1 ( renderPoint, editPoint, pointSize, grabDistance, curveColor
               , renderCurve, editCurve, graphPaper, whiteIm
               , editor, main
               ) where

import Editor0 hiding (main)

import Fran
import qualified StaticTypes as S
import FileUtils (load)
import InputMonitor

-- The looks of a curve.  The control points over a black polyBezier
renderCurve :: [Point2B] -> ImageB
renderCurve points =
  overs (map renderPoint points)            `over`
  withColor curveColor (polyBezier points)


curveColor = -- colorRGB256 0 100 0  -- dark green
             colorRGB256 0 0 139   -- dark blue
             -- purple


-- Produce a list of moving points from an initial point list and a user.
editCurve :: [S.Point2] -> User -> [Point2B]
editCurve initPoints u = map (editPoint u) initPoints


-- Simulated graph paper background
graphPaper, graphPaper' :: ImageB
graphPaper' = withColor lightBlue (horizontal `over` vertical) `over`
              whiteIm
 where
   horizontal = overs [ moveXY 0 (constantB y) hLine
                      | y <- [-2, -2+spacing .. 2]
                      ]
   vertical   = turn (pi/2) horizontal
   hLine      = lineSegment (point2XY (-2) 0) (point2XY 2 0)
   spacing    = 0.2

-- I displayed graphPaper', screen-captured the result, and saved it to
-- "graphPaper.bmp".  Someday, Fran will optimize well enough to make this
-- sort of thing unnecessary.  Shameful hack: I put one black pixel in the
-- upper left, so the white background wouldn't become transparent.
-- Transparency should really be specifiable.
--
-- Also (unrelated), the "line" primitive should be renamed "lineSegment",
-- and there should really be an infinite "line", which`` exploits cropping.

graphPaper = importBitmap "graphPaper.bmp"


-- Put it together.  Load file, edit, no save.
editor fileName = do
  initPoints <- load fileName
  displayUMon (\ u -> renderCurve (editCurve initPoints u) `over`
                      graphPaper)

main = editor "double.crv"
