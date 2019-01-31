-- Curve editor, version 6 with working "undo".
--
-- The trick is to move the undo stack out of the XPoint editor into the
-- curve editor.


module Editor6 ( XPoint, renderXPoint, editXPoint, editXPointTest
               , pointSize, grabDistance, graphPaper
               , renderCurve, EditCurve, editCurve
               , editorWith, editor, main
               ) where

-- Import all but editXPoint and derived functions.
import Editor4 hiding (editXPoint, editXPointTest, editCurve, editor, main)

import Fran
import qualified StaticTypes as S
import Stacker
import FileUtils


-- This time, pass in a resetTo event and pass out a changingFrom event.
editXPoint :: User -> Event S.Point2 -> S.Point2 -> (XPoint, Event S.Point2)
editXPoint u resetTo p0 = ((pos, closeEnough), grab)
 where
   pos = switcher (constantB p0) $
              grab `snapshot` mouse u ==> relMotion
          .|. release                 ==> constantB
    where
      relMotion (p, mp) = constantB p .+^ (mouse u .-. constantB mp)

   grab     = lbp u `whenE` closeEnough `snapshot_` pos
   release  = lbr u `whenE` grabbing    `snapshot_` pos
          .|. resetTo
   grabbing = stepper False (grab -=> True .|. release -=> False)

   closeEnough = distance2 pos (mouse u) <* grabDistance

-- Try it out: fake resetTo with rbp.  Discard changingFrom.
editXPointTest u = renderXPoint xp `over` graphPaper
 where
   (xp, changingFrom) = editXPoint u (rbp u `snapshot_` mouse u) S.origin2


-- Manage a shared undo stack that remembers which xPoint was grabbed and
-- its grab position.

type UndoRecord = (Int, S.Point2)

editCurve :: [S.Point2] -> User -> [XPoint]
editCurve initPoints u = xPoints
 where
   -- Tag and merge the XPoint grab events (defined below)
   curveGrab :: Event UndoRecord
   curveGrab = anyE (zipWith tag indices pointGrabs)
    where
      tag i e = e ==> (i `pair`)        -- pair i with e's occurrence data

   indices = [1 .. length initPoints]

   -- The undo event: stack curve grabs and try to restore on control-Z's
   undo :: Event UndoRecord
   undo = stacker curveGrab (charPress '\^Z' u)

   -- Edit an indexed XPoint.
   editXP :: Int -> S.Point2 -> (XPoint, Event S.Point2)
   editXP i p0 = editXPoint u undoThis p0
    where
      -- Undo if a point tagged i comes off the undo stack.  Drop tag.
      undoThis = undo `suchThat` ((== i) . fst) ==> snd

   -- Apply editXP to corresponding indices and initial points, and split
   -- (unzip) the resulting xPoints and grabs into two lists.
   (xPoints, pointGrabs) = unzip (zipWith editXP indices initPoints)


editor :: String -> IO ()
editor = editorWith "6" editCurve


main = editor "double.crv"
