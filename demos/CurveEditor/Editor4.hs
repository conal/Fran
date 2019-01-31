-- Curve editor, version 4.
--
-- The previous versions had a bug: control points recenter on the mouse
-- when grabbing.  This version gives them relative motion.

module Editor4 ( XPoint, renderXPoint, editXPoint, editXPointTest
               , pointSize, grabDistance, graphPaper, whiteIm
               , renderCurve, EditCurve, editCurve, spinMessage
               , editorWith, editor, main
               ) where

import Editor3 hiding (editXPoint, editCurve, editor, main)

import Fran
import qualified StaticTypes as S
import FileUtils
import InputMonitor
import Win32 (setWindowText)

editXPoint :: User -> S.Point2 -> XPoint
editXPoint u p0 = (pos, closeEnough)
 where
   -- Old version (locks onto mouse):
   --  pos = ifB grabbing (mouse u) lastRelease
   --  lastRelease = stepper p0 (release `snapshot_` pos)
   -- New version: relative motion
   pos = switcher (constantB p0) $
              grab `snapshot` mouse u ==> relMotion
          .|. release                 ==> constantB
    where
      relMotion (p, mp) = constantB p .+^ (mouse u .-. constantB mp)

      grab     = lbp u `whenE` closeEnough `snapshot_` pos
      release  = lbr u `whenE` grabbing    `snapshot_` pos
      grabbing = stepper False (grab -=> True .|. release -=> False)

   closeEnough = distance2 pos (mouse u) <* grabDistance

editXPointTest u = renderXPoint (editXPoint u S.origin2) `over`
                   whiteIm

-- Type of editCurve.  Used also in "editorWith" below.
type EditCurve = [S.Point2] -> User -> [XPoint]

editCurve :: EditCurve
editCurve initPoints u = map (editXPoint u) initPoints

-- No change needed for editor.  Instead of repeating the definition in
-- future versions, parameterize it by the version name and editCurve.

editorWith :: String -> EditCurve -> String -> IO ()
editorWith version editCurve fileName = do
  initPoints <- load fileName
  window     <- displayExMon (editRenderSave initPoints)
  setWindowText window ("Curve editor " ++ version ++ ": " ++ fileName)
  eventLoop window
 where
   editRenderSave initPoints u =
     ( spinMessage "Saving ..." saveNow `over`
       renderCurve xPoints              `over`
       graphPaper
     , const doSave )
    where
      xPoints = editCurve initPoints u
      pointsB :: Behavior [S.Point2]
      pointsB = bListToListB (map fst xPoints)

      saveNow = charPress 's' u .|. quit u
      doSave  = saveNow `snapshot_` pointsB
                        ==>         save fileName

editor :: String -> IO ()
editor = editorWith "4" editCurve


main = editor "double.crv"
-- main = editor "single.crv"
