-- Curve editor, version 2.  Adds visual feedback for point picking.

module Editor2 ( XPoint, renderXPoint, editXPoint, editXPointTest
               , pointSize, grabDistance, graphPaper, whiteIm
               , renderCurve, editCurve, editor, main
               ) where

import Editor1 hiding (renderCurve, editCurve, editor, main)

import Fran
import qualified StaticTypes as S
import FileUtils (load)
import InputMonitor

-- A "excitable point": position and whether excited.
type XPoint = (Point2B, BoolB)

-- The looks of an excitable point.  Green when excited, red when not.
renderXPoint :: XPoint -> ImageB
renderXPoint (pos, excited) =
  withColor (ifB excited excitedColor red) (
    renderPoint pos)
 where
   -- This definition must be local.  If it's a CAF, we get a bad
   -- space-time leak from the caching done by behaviors.  (The GC needs
   -- to be fixed to understand weak pair.)
   excitedColor = shade (faster 4 (wiggleRange 0.1 0.25)) green
                  -- colorHSL (5*time) 0.5 0.5
                  -- brighter (5*wiggle) red
                  -- shade (faster 7 (wiggleRange (-0.5) 0.5)) red
                  -- colorHSL
                  -- colorRGB256 0 255 0  -- dark green
               -- shade 0.15 green

-- Test: tracks mouse position and left button state
renderXPointTest u = renderXPoint (mouse u, leftButton u)


-- Excitable point editor.  Identical to Editor1.editPoint, but exports
-- closeEnough.
editXPoint :: User -> S.Point2 -> XPoint
editXPoint u p0 = (pos, closeEnough)
 where
   -- Track mouse while grabbing; otherwise last release position.
   pos = ifB grabbing (mouse u) lastRelease

   grabbing = stepper False (grab -=> True .|. release -=> False)
   grab     = lbp u `whenE` closeEnough
   release  = lbr u `whenE` grabbing
   closeEnough = distance2 pos (mouse u) <* grabDistance

   -- Position of last release.  Start point at first, then snapshot the
   -- point position whenever released
   lastRelease = stepper p0 (release `snapshot_` pos)

-- Try it out
editXPointTest u = renderXPoint (editXPoint u S.origin2) `over`
                   whiteIm


-- Curve editing and rendering is now in terms of xPoints.

editCurve :: [S.Point2] -> User -> [XPoint]
editCurve initPoints u = map (editXPoint u) initPoints

renderCurve :: [XPoint] -> ImageB
renderCurve xPoints =
  overs (map renderXPoint xPoints)                     `over`
  withColor curveColor (polyBezier (map fst xPoints))

editor :: String -> IO ()
editor fileName = do
  initPoints <- load fileName
  displayUMon ((`over` graphPaper) . renderCurve . editCurve initPoints)


main = editor "double.crv"
