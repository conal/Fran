-- Curve editor version 8.  Keeps track of insertion/deletion point.
-- Initially no undo.
--
-- I'm not satisfied with this one yet, because:
-- + Rebuilding the whole point list for a single insert or delete seems
--   monolithic and inefficient.  (On the other hand, the points do shift.)
-- + Needs error checking for moving the insert/delete point.

module Editor8 ( XPoint, renderXPoint, editXPoint, editXPointTest
               , pointSize, grabDistance, graphPaper
               , renderCurve, EditCurve, editCurve
               , ECurve, renderECurve, editECurve
               , editorWith, editor, main
               ) where

import Editor7 hiding (editECurve, editorWith, editor, main)

import Fran
import qualified StaticTypes as S
import FileUtils
import InputMonitor
import Win32Key

-- Type of editCurve.  Used also in "editorWith" below.
type EditECurve = [S.Point2] -> User
               -> (ECurve, Behavior [S.Point2], IntB)

editECurve :: EditECurve
editECurve initPoints u = ((firstCurve, newCurve), pointsB, pointToEdit)
 where
   firstCurve = editCurve initPoints u

   newCurve :: Event [XPoint]
   newCurve = change `snapshot` pairB pointToEdit pointsB
                     ==>        uncurry ($)
                     `afterE`   u
                     ==>        uncurry editCurve

   change :: Event ((Int, [S.Point2]) -> [S.Point2])
   change =     charPress 'i'       u -=> insertPoint
            .|. keyPress  vK_DELETE u -=> deletePointsForward
            .|. keyPress  vK_BACK   u -=> deletePointsBackward

   pointsB = mapSwitcher (bListToListB . map fst) firstCurve newCurve

   pointToEdit = clampSeg $
                 stepper 0 (moveEdit `snapshot` pointToEdit
                                      ==>       uncurry (+))

   moveEdit =      keyPress vK_RIGHT u -=>  3
              .|.  keyPress vK_LEFT  u -=> -3

   clampSeg = lift3 clamp 0 (lift1 length pointsB)
    where
      clamp a b n = a `max` (b `min` n)

insertPoint, deletePointsForward, deletePointsBackward
  :: (Int, [S.Point2]) -> [S.Point2]

insertPoint (beforePoint, points) =
  recenter (prefix ++ newPoints ++ map shift suffix)
 where
   prefix = take beforePoint points
   suffix = drop beforePoint points
   S.Point2XY lx ly = head suffix
   newPoints = [S.Point2XY (lx + dx * fromInt i) ly | i <- [1 .. 3]]
   dx = 0.3

   shift (S.Point2XY x y) = S.Point2XY (x + dx * 4) y

-- Shift a list of points so that their bounding box is centered on the
-- origin.
recenter :: [S.Point2] -> [S.Point2]
recenter ps = map (S..-^ shift) ps
 where
   shift = S.vector2XY ((maximum xs + minimum xs) / 2)
                       ((maximum ys + minimum ys) / 2)
   (xs, ys) = unzip (map S.point2XYCoords ps)

      
deletePointsForward (beforePoint, points)
  | beforePoint < length points - 1 
    = take beforePoint points ++ drop (beforePoint+3) points
  | otherwise = points

deletePointsBackward (beforePoint, points)
  | beforePoint > 0 = deletePointsForward (beforePoint-3, points)
  | otherwise       = points


editorWith :: String -> EditECurve -> String -> IO ()
editorWith version editECurve fileName = do
  initPoints <- load fileName
  window     <- displayExMon (editRenderSave initPoints)
  setWindowTextA window ("Curve editor " ++ version ++ ": " ++ fileName)
  eventLoop window
 where
   editRenderSave initPoints u =
     ( spinMessage "Saving ..." saveNow `over`
       editMarker                       `over`
       renderECurve eCurve              `over`
       graphPaper
     , const doSave )
    where
      (eCurve, pointsB, pointToEdit) = editECurve initPoints u

      saveNow = charPress 's' u .|. quit u
      doSave  = saveNow `snapshot_` pointsB ==> save fileName

      editMarker = moveTo (pointsB !!* pointToEdit) $
                   withColor yellow                         $
                   stretch 0.1                              $
                   star 3 7

editor :: String -> IO ()
editor = editorWith "8" editECurve


main = editor "single.crv"
