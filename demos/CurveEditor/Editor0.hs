-- Version 0.  A simple point editor, allowing the user to grab it with
-- the left mouse button and move it around.

module Editor0 where

import Fran
import qualified StaticTypes as S


-- Initially we will represent control points by Fran 2D points.
type CPoint  = S.Point2
type CPointB = Point2B

-- The looks of a control point
renderPoint :: CPointB -> ImageB
renderPoint pos = moveTo pos (stretch pointSize circle)

pointSize :: RealB
pointSize =  0.07                           -- size of point image

-- Test: tracks mouse position.  Use "displayU renderPoint" to test
renderPointTest u = renderPoint (mouse u)

-- Point editor.  From a start point, come up with a moving point
-- under user control.  User can grab with left button when
-- the mouse is close enough (within two radii).
editPoint :: User -> CPoint -> CPointB
editPoint u p0 = pos
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

grabDistance :: RealB
grabDistance = 2 * pointSize                    -- proximity for grabbing

-- Try it out
editPointTest u = renderPoint (editPoint u S.origin2) `over`
                  whiteIm

whiteIm = withColor white solidImage

main = displayUMon editPointTest
