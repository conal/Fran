-- Source code for UIST'98 submission

module Uist where

import Fran
import qualified StaticTypes as S
import NewCurve

-- A control point is a Fran 2D point behavior and a boolean behavior
-- saying whether grabbable.
type CPoint  = (Point2B, BoolB)

-- The looks of a control point

renderPoint :: Point2B -> ImageB
renderPoint pos =
  moveTo pos (stretch pointSize circle)

pointSize :: RealB
pointSize =  0.07

renderCPoint :: CPoint -> ImageB
renderCPoint (pos, grabbable) =
  withColor (ifB grabbable green red)
            (renderPoint pos)

-- Control point editor.  From a start point, come up with a moving point
-- under user control.  User can grab with left button when the mouse is
-- close enough (within two radii).
editCPoint :: User -> S.Point2 -> CPoint
editCPoint u p0 = (pos, closeEnough)
 where
   pos, lastRelease :: Point2B
   pos = ifB grabbing (mouse u) lastRelease
   lastRelease = stepper p0 (release `snapshot_` pos)

   closeEnough, grabbing :: BoolB
   closeEnough = distance2 pos (mouse u) <* grabDistance
   grabbing = stepper False (grab -=> True .|. release -=> False)

   grab, release :: Event ()
   grab     = lbp u `whenE` closeEnough
   release  = lbr u `whenE` grabbing

grabDistance :: RealB
grabDistance = 2 * pointSize                    -- proximity for grabbing

-- Try it out
editCPointTest :: User -> ImageB
editCPointTest u = renderCPoint (editCPoint u S.origin2) `over` whiteIm

whiteIm = withColor white solidImage

main1 = displayUMon editCPointTest


renderCurve :: [CPoint] -> ImageB
renderCurve cpoints =
  overs (map renderCPoint cpoints)   `over`
  withColor blue (polyBezier (map fst cpoints))

editCurve :: [S.Point2] -> User -> [CPoint]
editCurve initPoints u = map (editCPoint u) initPoints


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

editor :: [S.Point2] -> User -> ImageB
editor initPoints u =
  renderCurve (editCurve initPoints u) `over` graphPaper

main2 = displayUMon (editor (sinPoints 2))


-- Now add undo

-- Polymorphic, unbounded "stack".  Feed pushes and pop attempts in, and
-- get successful pops out.
stacker :: Event a -> Event () -> Event a
stacker push tryPop = legitPop `snapshot_` headB stack
 where
   legitPop :: Event ()
   legitPop = tryPop `whenE` notB (nullB stack)
   -- changeStack :: Event ([a] -> [a])
   changeStack = legitPop -=> tail .|. push ==> (:)
   -- stack :: Behavior [a]
   stack = stepAccum [] changeStack

-- This one has a problem (see text)
editCPointUndo1 :: User -> S.Point2 -> CPoint
editCPointUndo1 u p0 = (pos, closeEnough)
 where
   pos, lastRelease :: Point2B
   pos = ifB grabbing (mouse u) lastRelease
   lastRelease = stepper p0 (release `snapshot_` pos .|. undo)

   closeEnough, grabbing :: BoolB
   closeEnough = distance2 pos (mouse u) <* grabDistance
   grabbing = stepper False (grab -=> True .|. release -=> False)

   grab, release :: Event ()
   grab     = lbp u `whenE` closeEnough
   release  = lbr u `whenE` grabbing

   grabPos, undo :: Event S.Point2
   grabPos = grab `snapshot_` pos
   undo    = stacker grabPos (charPress '\^Z' u)

-- For visualizing the moving point and the undo stack.
editCPointUndo1Test u = renderCPoint (editCPointUndo1 u S.origin2) `over`
                        whiteIm

main3 = displayUMon editCPointUndo1Test


-- Fixed version.

-- This one has a problem (see text)
editCPointUndo :: User -> Event S.Point2 -> S.Point2
               -> (CPoint, Event S.Point2)
editCPointUndo u undo p0 = ((pos, closeEnough), grabPos)
 where
   pos = ifB grabbing (mouse u) lastRelease
   lastRelease = stepper p0 (release `snapshot_` pos .|. undo)

   closeEnough = distance2 pos (mouse u) <* grabDistance
   grabbing = stepper False (grab -=> True .|. release -=> False)

   grab     = lbp u `whenE` closeEnough
   release  = lbr u `whenE` grabbing

   grabPos  = grab `snapshot_` pos


-- Manage a shared undo stack that remembers which xPoint was grabbed and
-- its grab position.

type UndoRecord = (Int, S.Point2)

editCurveUndo :: [S.Point2] -> User -> [CPoint]
editCurveUndo initPoints u = cpoints
 where
   -- Tag and merge the CPoint grab events (defined below)
   curveGrab :: Event UndoRecord
   curveGrab = anyE (zipWith tag indices pointGrabs)
    where
      tag i e = e ==> (i `pair`)        -- pair i with e's occurrence data

   indices = [1 .. length initPoints]

   -- The undo event: stack curve grabs and try to restore on control-Z's
   undo :: Event UndoRecord
   undo = stacker curveGrab (charPress '\^Z' u)

   -- Edit an indexed CPoint.
   editCP :: Int -> S.Point2 -> (CPoint, Event S.Point2)
   editCP i p0 = editCPointUndo u undoThis p0
    where
      -- Undo if a point tagged i comes off the undo stack.  Drop tag.
      undoThis = undo `suchThat` ((== i) . fst) ==> snd

   -- Apply editCP to corresponding indices and initial points, and split
   -- (unzip) the resulting cpoints and grabs into two lists.
   (cpoints, pointGrabs) = unzip (zipWith editCP indices initPoints)


editorUndo :: [S.Point2] -> User -> ImageB
editorUndo initPoints u =
  renderCurve (editCurveUndo initPoints u) `over` graphPaper

main4 = displayUMon (editorUndo (sinPoints 2))
