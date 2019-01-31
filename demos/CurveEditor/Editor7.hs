-- Curve editor version 7.  Adds simple insertion of curve segments.
-- No undo.

module Editor7 ( XPoint, renderXPoint, editXPoint, editXPointTest
               , pointSize, grabDistance, graphPaper
               , renderCurve, EditCurve, editCurve, spinMessage
               , ECurve, renderECurve, editECurve
               , editorWith, editor, main
               ) where

import Editor4 hiding (editorWith, editor, main)

import Fran
import qualified StaticTypes as S
import FileUtils
import InputMonitor

-- Editable curve.   (BTW, there's a very common pattern here.)
type ECurve = ([XPoint], Event [XPoint])

renderECurve :: ECurve -> ImageB
renderECurve (startXPoints, changeXPoints) =
  mapSwitcher renderCurve startXPoints changeXPoints

-- Type of editCurve.  Used also in "editorWith" below.
type EditECurve = [S.Point2] -> User -> (ECurve, Behavior [S.Point2])

editECurve :: EditECurve
editECurve initPoints u = ((firstCurve, newCurve), pointsB)
 where
   firstCurve = editCurve initPoints u

   newCurve :: Event [XPoint]
   newCurve = charPress 'i' u `snapshot_` pointsB
                              ==>         addSegment
                              `afterE`    u
                              ==>         uncurry editCurve

   pointsB = mapSwitcher (bListToListB . map fst) firstCurve newCurve


-- Add new segment at the end and recenter.
addSegment :: [S.Point2] -> [S.Point2]
addSegment points = recenter (points ++ newOnes)
 where
   S.Point2XY lx ly = last points
   newOnes = [S.Point2XY (lx + dx * fromInt i) ly | i <- [1 .. 3]]
   dx      = 0.3

   recenter ps = map (S..-^ shift) ps
    where
      shift = S.vector2XY ((maximum xs + minimum xs) / 2)
                          ((maximum ys + minimum ys) / 2)
      (xs, ys) = unzip (map S.point2XYCoords ps)
      

editorWith :: String -> EditECurve -> String -> IO ()
editorWith version editECurve fileName = do
  initPoints <- load fileName
  window     <- displayExMon (editRenderSave initPoints)
  setWindowTextA window ("Curve editor " ++ version ++ ": " ++ fileName)
  eventLoop window
 where
   editRenderSave initPoints u =
     ( spinMessage "Saving ..." saveNow `over`
       renderECurve eCurve              `over`
       graphPaper
     , const doSave )
    where
      (eCurve, pointsB) = editECurve initPoints u

      saveNow = charPress 's' u .|. quit u
      doSave  = saveNow `snapshot_` pointsB ==> save fileName

editor :: String -> IO ()
editor = editorWith "7" editECurve


main = editor "single.crv"
