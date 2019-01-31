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
import Win32Key

-- Editable curve.  A list of x-points, and an change event.  (BTW,
-- there's a very common pattern here that should be captured in a type.)
type ECurve = ([XPoint], Event [XPoint])

renderECurve :: ECurve -> ImageB
renderECurve (startXPoints, changeXPoints) =
  mapSwitcher renderCurve startXPoints changeXPoints

-- Type of editCurve.  Used in "editorWith" below.  Yields 
type EditECurve = [S.Point2] -> User
               -> (ECurve, Behavior [S.Point2], IntB)

editECurve :: EditECurve
editECurve initPoints u = ((firstCurve, newCurve), pointsB, pointToEdit)
 where

   change :: Event ((Int, [S.Point2]) -> [S.Point2])
   change =     okInsert   -=> insertPoint
            .|. okDelRight -=> deletePointsForward
            .|. okDelLeft  -=> deletePointsBackward

   okInsert   = charPress 'i'       u
   okDelRight = keyPress  vK_DELETE u `whenE` beforeEnd
   okDelLeft  = keyPress  vK_BACK   u `whenE` afterStart

   beforeEnd  = pointToEdit <* lengthB pointsB - 1
   afterStart = pointToEdit >* 0

   firstCurve = editCurve initPoints u

   newCurve :: Event [XPoint]
   newCurve = change `snapshot` pairB pointToEdit pointsB
                     ==>        uncurry ($)
                     ==>        recenter
                     `afterE`   u
                     ==>        uncurry editCurve

   pointToEdit = stepper 0 (moveEdit ==>        (* 3)
                                     `snapshot` pointToEdit
                                     ==>        uncurry (+))

   moveEdit = okRight -=>  1 .|. (okLeft .|. removedLast) -=> -1

   okRight = keyPress vK_RIGHT u `whenE` beforeEnd
   okLeft  = keyPress vK_LEFT  u `whenE` afterStart
   removedLast = okDelLeft `whenE` notB beforeEnd

   pointsB = mapSwitcher (bListToListB . map fst) firstCurve newCurve


insertPoint, deletePointsForward, deletePointsBackward
  :: (Int, [S.Point2]) -> [S.Point2]

insertPoint (beforePoint, points) =
  prefix ++ newPoints ++ map (S..+^ shift) suffix
 where
   prefix = take beforePoint points
   suffix = drop beforePoint points
   S.Point2XY lx ly = head suffix
   newPoints = [S.Point2XY (lx + dx * fromInt i) ly | i <- [1 .. 3]]
   dx = 0.3
   shift = S.vector2XY (dx * 4) 0
      
deletePointsForward (beforePoint, points) =
  prefix ++ map (S..+^ shift) suffix
 where
   prefix = take  beforePoint    points
   suffix = drop (beforePoint+3) points
   -- Shift the suffix points to make the first one line up with
   -- the first deleted point.
   shift  = (points !! beforePoint) S..-. head suffix

deletePointsBackward (beforePoint, points) =
  deletePointsForward (beforePoint-3, points)

-- Shift a list of points so that their bounding box is centered on the
-- origin.
recenter :: [S.Point2] -> [S.Point2]
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
       editMarker                       `over`
       renderECurve eCurve              `over`
       graphPaper
     , const doSave )
    where
      (eCurve, pointsB, pointToEdit) = editECurve initPoints u

      saveNow = charPress 's' u .|. quit u
      doSave  = saveNow `snapshot_` pointsB ==> save fileName

      editMarker = moveTo (pointsB !!* pointToEdit) $
                   withColor yellow                 $
                   stretch 0.1                      $
                   star 3 7

editor :: String -> IO ()
editor = editorWith "8" editECurve

lengthB = lift1 length

main = editor "double.crv"
